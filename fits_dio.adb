--
-- Implementation notes:
--
-- FIXME shouldn't FitsFile : File_Type be 'in out' ? we update the Index...
--
-- Re-consider API:
--   Offset in Set_Index() should be in 'Read() 'Write().
--   Motivation:
--   concept of Block is part of the FITS standard.
--   This API would support read/write in blocks -
--   e.g. always guarantee naturally correct size and fill-in areas.
--   If Offset is in Set_Index only Stream_IO is supported.
--   Interfaces like Direct_IO(Block) are not.
--   E.g. such new API:
--   procedure Stream(File_Type; HDUNum) <- position to begining of HDU
--   procedure used for 'Read (FITSStream, FITS_Data_Type, Offset )
--   procedure used for 'Write(FITSStream, FITS_Data_Type, Offset )
--   Drawback:
--   Read/Write of DataUnit would need to calculate Header size
--   at first call of multiple writes. Successive writes can be
--   done directly by Stream: 'Write/'Read(FITSStream, Data).
--   After first read/write we are correctly positioned, and read/write
--   move the file pointer.
--   For multiple writes (reads) to (from) continuous area,
--   a client would do:
--   FIST_Data_Type'Write(FITS, Data(1), Offset); <-- func from FITS package with initial positioning
--   FIST_Data_Type'Write(FITS, Data(2) );        <-- func from Stream_IO, only write, no positioning
--   FIST_Data_Type'Write(FITS, Data(3) );
--   ... n-times
--

with Ada.Text_IO;

with Ada.Unchecked_Conversion;

-- with Ada.Streams.Stream_IO;
-- use  Ada.Streams.Stream_IO;
-- 'use' needed for + operator on Count type and all its subtypes(?)

package body FITS_DIO is

   procedure Move_Index
             (FitsFile : in BIO.File_Type;
              ByCount  : in BIO.Positive_Count) is
   begin
     BIO.Set_Index(FitsFile, BIO.Index(FitsFile) + ByCount);
   end Move_Index;
   pragma Inline (Move_Index);
   -- util: consider this part of Stream_IO

   -- FITS itself:

   StreamElemSize_bits : FPositive := 8;-- FIXME Ada.Streams.Stream_Element'Size;
    -- FIXME [GNAT somwhere says it is 8bits]
    -- [GNAT]:
    --  type Stream_Element is mod 2 ** Standard'Storage_Unit;
    -- (Storage_Unit a.k.a 'Byte' : smallest addressable unit)

   BlockSize_bits  : FPositive := 2880*8;-- needed because of BITPIX is size in bits
--   BlockSize_bytes : FPositive := 2880*8 / StreamElemSize_bits;
   -- FIXME division : needs to be multiple of another otherwise
   --                  fraction lost
   -- in units of Stream_Element size (usually octet-byte)
   -- which is unit for positioning in Stream_IO by Set_Index() & Index()


   type Unit_Type is (HeaderUnit, DataUnit);

   -- from Data-type decide whether it is for Header or Data Unit
   --  arrays derived from Character are for Header Read/Write
   --  other (arrays derived from BITPIX) are for Data Unit access
   function  To_UnitType (DataType : in FITSData_Type) return Unit_Type
   is
    UType : Unit_Type := HeaderUnit;
   begin

    case DataType is
    when Int8 | Int16 | Int32 | Int64 | Float32 | Float64 =>
     UType := DataUnit;
    when others =>
     UType := HeaderUnit;
    end case;

    return UType;
   end To_UnitType;


   --
   -- convert BITPIX keyword from Header to internal FITSData_Type
   --
   function  To_FITSDataType (BITPIX : in Integer ) return FITSData_Type
   is
    bp : FITSData_Type;
   begin
    case BITPIX is
    when   8 => bp := Int8;
    when  16 => bp := Int16;
    when  32 => bp := Int32;
    when  64 => bp := Int64;
    when -32 => bp := Float32;
    when -64 => bp := Float64;
    when others =>
     null;
     -- FIXME ? raise exception "out of range"
    end case;
    return bp;
   end To_FITSDataType;


   --
   -- calculate DataUnit size in FITS Blocks
   --
   function  Size_Blocks (DUSizeParam : in out DUSizeParam_Type) return FPositive
   is
    DataInBlock    : FPositive;
    DUSizeInBlocks : FPositive;
    DUSize         : FPositive := 1;
   begin

     for I in 1..DUSizeParam.Naxes
     loop
      DUSize := DUSize * DUSizeParam.Naxis(I);
     end loop;
      -- DUSize cannot be 0: Naxis(I) is FPositive
      -- cannot be 0 (parsing would throw exception)

     DataInBlock := BlockSize_bits /  FNatural( abs DUSizeParam.BITPIX );
     -- per FITS standard, these vlues are multiples

     DUSizeInBlocks := (DUSize - 1) / DataInBlock + 1;

    return DUSizeInBlocks;
   end Size_Blocks;
   pragma Inline (Size_Blocks);


   -- parse from Card value if it is one of DUSizeParam_Type, do nothng otherwise
   -- and store parse value to DUSizeParam
   -- TODO what to do if NAXIS and NAXISnn do not match in a broken FITS-file
   -- [FITS,Sect 4.4.1.1]: NAXISn keys _must_ match NAXIS keyword.
   procedure Parse_Card (Card        : in Card_Type;
                         DUSizeParam : in out DUSizeParam_Type)
   is
    dim : Positive;
   begin
     -- FIXME what if parsed string is '' or '     ' etc...

     -- [FITS 4.1.2 Components]:
     -- pos 9..10 is '= '
     -- pos 31 is comment ' /'
     -- then : pos 10..20 is value
     if    (Card(1..9) = "BITPIX  =") then
       DUSizeParam.Data   := To_FITSDataType(Integer'Value(Card(10..30)));
       DUSizeParam.BITPIX := Integer'Value(Card(10..30));

     elsif (Card(1..5) = "NAXIS") then

       if (Card(1..9) = "NAXIS   =") then
           DUSizeParam.Naxes := Positive'Value(Card(10..30));
       else
           dim := Positive'Value(Card(6..8));
           DUSizeParam.Naxis(dim) := FPositive'Value(Card(10..30));
           -- FIXME [FITS Section??: NAXISn is always positive and zero]
           -- [FITS fixed integer]:
   	   -- Theoretical problem: Fixed integer is defined as 19 decimal digits
   	   -- (Header Card Integer value occupying columns 11..20)
   	   -- Lon_Long_Integer in GNAT is 64bit: 9.2 x 10**19 whereas
   	   -- fixed integer can reach 9.9 x 10**19)
           -- Conclude: range of NAXISn will be implementation limited.
       end if;

     end if;

   end Parse_Card;


   function  To_HeaderBlock is
    new Ada.Unchecked_Conversion(Source => Block_Type, Target => HeaderBlock_Type);
   -- actually here we should do cycle and handle low level things like
   -- Endianness, alignment somehow also BZERO and BFACT FITS-keywords(?)


   -- parse one header for HDU-size information
   --  from current file-index,
   --  read cards until END-card found and try to fill in HDUSize
   procedure Parse_Header (FitsFile : in BIO.File_Type;
                           HDUSize  : in out HDU_Size_Type)
   is
    Card : Card_Type;
    Block : Block_Type;
    HBlock : HeaderBlock_Type;
    ENDCardFound : Boolean := False;
   begin
     HDUSize.CardsCnt := 1;

     while not ENDCardFound
     loop

       BIO.Read(FitsFile, Block);
       HBlock := To_HeaderBlock (Block);

       for I in HBlock'Range
       loop
         Card := HBlock(I);
         Parse_Card (Card, HDUSize.DUSizeParam);
         ENDCardFound := (Card = ENDCard);
--         Ada.Text_IO.Put_Line(Boolean'Image(ENDCardFound) &
--                              " " & Positive'Image(HDUSize.CardsCnt) &
--                              " >>" & Card );
         exit when ENDCardFound;
         HDUSize.CardsCnt := HDUSize.CardsCnt + 1;
       end loop; -- HBlock loop

     end loop; -- ENDCardFound loop

   end Parse_Header;


   --
   -- Set file index to position given by params
   --
   procedure Set_Index (FitsFile : in BIO.File_Type;
                        HDUNum   : in Positive;      -- which HDU
                        DataType : in FITSData_Type; -- decide to position to start of HeaderUnit or DataUnit
                        Offset   : in FNatural := 0)  -- offset within the Unit (in units of FITSData_Type)
   is
    CurDUSize_blocks : FPositive;
    Offset_bytes     : FNatural;
    CurHDUNum : Positive := 1;
    HDUSize   : HDU_Size_Type;
   begin

    BIO.Set_Index(FitsFile, 1);
    -- move to begining of the Primary HDU

    while CurHDUNum < HDUNum
    loop
     -- move past current Header
     Parse_Header(FitsFile, HDUSize);

     -- skip DataUnit if exists
     if HDUSize.DUSizeParam.Naxes /= 0
     then
       CurDUSize_blocks := Size_Blocks (HDUSize.DUSizeParam);
       Move_Index(FitsFile, BIO.Positive_Count(CurDUSize_blocks));
     end if;

     -- next HDU
     CurHDUNum := CurHDUNum + 1;
    end loop;

    -- if DataType indicates DataUnit move past current Header
    if DataUnit = To_UnitType(DataType)
    then
     -- move past current Header
     Parse_Header(FitsFile, HDUSize);
    end if;

    -- add Offset
    if Offset /= 0
    then
     Offset_bytes := Offset * DataType'Size / StreamElemSize_bits;
      -- explicit conversions Natural -> Count ok:
      -- it is under if then... cannot be zero.
     Move_Index(FitsFile,BIO.Positive_Count(Offset_bytes));
    end if;

   end Set_Index;


   --
   -- List size-related params of HDU
   --
   procedure List_Content (FitsFile : in BIO.File_Type;
                           Print: not null access
                             procedure(HDUNum : Positive; HDUSize : HDU_Size_Type))
   is
    HDUCnt  : Positive := 1;
    HDUSize : HDU_Size_Type;
    CurDUSize_blocks : FPositive;
    CurIndex  : BIO.Count := 0;
   begin

    -- start from begining
    BIO.Set_Index(FitsFile,1);

    while not BIO.End_Of_File(FitsFile)
    loop
     -- move past current Header
     Parse_Header(FitsFile, HDUSize);

     -- do the callback
     Print(HDUCnt,HDUSize);

     -- skip DataUnit if exists
     if HDUSize.DUSizeParam.Naxes /= 0
     then
       CurDUSize_blocks := Size_Blocks(HDUSize.DUSizeParam);
       Move_Index(FitsFile, BIO.Positive_Count(CurDUSize_blocks));
     end if;

     -- next HDU
     HDUCnt := HDUCnt + 1;
    end loop;

   end List_Content;

end FITS_DIO;

