

-- FIXME shouldn't FitsFile : File_Type be 'in out' ? we update the Index...

-- for debug only
with Ada.Text_IO;



-- with Interfaces;
with Ada.Streams.Stream_IO;
use  Ada.Streams.Stream_IO; -- needed for + operator on Count type

package body FITS is

   StreamElemSizeInBits : Positive := Ada.Streams.Stream_Element'Size;
    -- FIXME [GNAT somwhere says it is 8bits]
    -- [GNAT]:  type Stream_Element is mod 2 ** Standard'Storage_Unit;
    -- (Storage_Unit a.k.a 'Byte' on this PC-machine 8bits)


   BlockSizeInOct   : Positive :=  2880; -- octets [FITS block size]
   BlockSizeInBits  : Positive :=  2880*8;
   BlockSizeInBytes : Positive := (2880*8) / StreamElemSizeInBits;
   -- GNAT derives Stream_Element from Storage_Unit (a.k.a. 'Byte': smallest addressable unit)
   -- FIXME division : needs to be multiple of another otherwise
   --                  fraction lost
   -- in units of Stream_Element size (usually octet-byte)
   -- which is unit for positioning in Stream_IO by Set_Index() & Index()


   type Unit_Type is (HeaderUnit, DataUnit);

   -------------------------------------
   -- from Data-type decide whether it is for Header or Data Unit
   --  arrays derived from Character are for Header Read/Write
   --  other (arrays derived from BITPIX) are for Data Unit access
   function  To_UnitType(DataType : in FITSData_Type) return Unit_Type
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

   -------------------------------------
   -- calculate DataUnit size in Bits
   -- implements [FITS 4.4.1.1 Primary Header (1)]
   -- FIXME is the return type Natural enough? Derive from File-index type!
   function  Size_Bits(DUSizeParam : in out DUSizeParam_Type) return FNatural
   is
    DUSize     : FNatural := 1;
    BITPIXSize : FNatural := FNatural(abs (DUSizeParam.BITPIX));
    -- BITPIX carries size in bits [FITS ??]
    -- conversion ok: abs always positive, max Val << FNAtural'Last
   begin

     for I in 1..DUSizeParam.Naxes
     loop
      DUSize := DUSize * FNatural(DUSizeParam.Naxis(I));
      -- explicit converion ok Positive -> Natural FIXME couldnt this be done with subtype decl?
     end loop;

     Ada.Text_IO.Put_Line("DBG> DUSize: " &
                           FNatural'Image(DUSize) &
                           "    BITPIXSize: " & FNatural'Image(BITPIXSize));

    return (DUSize*BITPIXSize);
   end Size_Bits;

   pragma Inline (Size_Bits);


   -----------------------------------------
   -- calculate DataUnit size in FITS Blocks
   function  Size_Blocks(DUSizeParam : in out DUSizeParam_Type) return FNatural
   is
    DataInBlock : FNatural;
    DUSizeInBlocks : FNatural;
    DUSize     : FNatural := 1;
   begin

     for I in 1..DUSizeParam.Naxes
     loop
      DUSize := DUSize * FNatural(DUSizeParam.Naxis(I));
                     -- explicit conversion ok bigger ange to smaller range,
                     -- FIXME couldnt this be done with subtype decl?
     end loop;
     -- DUSize can 0 if Naxis from FITS-file is
     -- FIXME consider: define FPositive and let throw exception
     -- when we parse zero

     DataInBlock := FNatural( BlockSizeInBits / abs DUSizeParam.BITPIX );
     -- per FITS standard, these vlues are multiples

     DUSizeInBlocks := (DUSize - 1) / DataInBlock + 1;
     -- if DUSize is 0, this is negative and throws excpetion

    return DUSizeInBlocks;
   end Size_Blocks;

   pragma Inline (Size_Blocks);


   -------------------------------------
   -- convert BITPIX keyword from Header to internal FITSData_Type
   function  To_FITSDataType( BITPIX : in Integer )
    return FITSData_Type
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

   -------------------------------------
   -- parse from Card value if it is one of DUSizeParam_Type, do nothng otherwise
   -- and store parse value to DUSizeParam
   -- TODO what to do if NAXIS and NAXISnn do not match in a broken FITS-file
   -- [FITS,Sect 4.4.1.1]: NAXISn keys _must_ match NAXIS keyword.
   procedure Parse_Card(Card        : in Card_Type;
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
       end if;

     end if;

   end Parse_Card;

   -------------------------------------
   -- parse one header for HDU-size information
   --  from current file-index,
   --  read cards until END-card found and try to fill in HDUSize
   procedure Parse_Header(FitsFile : in SIO.File_Type;
                          HDUSize  : in out HDU_Size_Type)
   is
    Card : Card_Type;
    FreeSlotCnt : Natural;
   begin

    Card_Type'Read( Stream(FitsFile), Card );
    HDUSize.CardsCnt := 1;

    while Card /= ENDCard
    loop
      Parse_Card (Card, HDUSize.DUSizeParam);
      Card_Type'Read( Stream(FitsFile), Card );
      HDUSize.CardsCnt := HDUSize.CardsCnt + 1;
    end loop;

    -- calc free slots
    FreeSlotCnt := 36 - (HDUSize.CardsCnt mod 36);
    -- mod is 0 when Block has 36 cards e.g. is full
    if FreeSlotCnt = 36 then
     FreeSlotCnt := 0;
    end if;

    -- read up to block-limit
    while FreeSlotCnt /= 0
    loop
      Card_Type'Read( Stream(FitsFile), Card );
      FreeSlotCnt := FreeSlotCnt - 1;
    end loop;

   end Parse_Header;

   -- Skip DataUnit.
   -- Before this call file's internal pointer must
   -- be pointing to the begining of the DataUnit
   procedure Move_Index_Behind_DataUnit
             (FitsFile       : in SIO.File_Type;
              DUSizeInBlocks : in FNatural)
   is
    CurIndex : SIO.Count := SIO.Index(FitsFile);
    ToInx :  SIO.Count;
   begin

     ToInx := CurIndex +
              SIO.Count(DUSizeInBlocks * FNatural(BlockSizeInBytes));
      -- FIXME can this happen? If filesystem does not
      -- support file that bigger then Count'Last?
      -- FIXME check explicit conversion FNatural -> SIO.Count:
      -- FNatural defined by 19 decimal digits -> close to 64bit Integer
      -- In GNAT, SIO.Count is 32bit or 64bit :
      --  type Count is new Stream_Element_Offset
      --                range 0 .. Stream_Element_Offset'Last;
      --  type Stream_Element_Offset is range
      --               -(2 ** (Standard'Address_Size - 1)) ..
      --               +(2 ** (Standard'Address_Size - 1)) - 1;
      -- Address_Size is 32 or 64bit nowadays

--     Ada.Text_IO.Put_Line(
--        "DBG> CurIndex:          " &
--        SIO.Count'Image(CurIndex));
     Ada.Text_IO.Put_Line(
        "DBG> ToIndex:           " &
        SIO.Count'Image(ToInx));
     Ada.Text_IO.Put_Line(
        "     SIO.Count'Last:    " &
        SIO.Count'Image(SIO.Count'Last) );
--     Ada.Text_IO.Put_Line(
--        "DBG> DUSizeInBytes:      " &
--        FNatural'Image(DUSizeInBlocks * FNatural(BlockSizeInBytes)) );
--     Ada.Text_IO.Put_Line(
--        "Long_Long_Integer'Last: " &
--        Long_Long_Integer'Image(Long_Long_Integer'Last));

     SIO.Set_Index(FitsFile,ToInx);
   end Move_Index_Behind_DataUnit;

   -------------------------------------
   -- Set file index to position given by params
   procedure Set_Index(FitsFile : in SIO.File_Type;
                       HDUNum   : in Positive;      -- which HDU
                       DataType : in FITSData_Type; -- decide to position to start of HeaderUnit or DataUnit
                       Offset   : in Natural := 0)  -- offset within the Unit (in units of FITSData_Type)
                       -- FIXME Offset should be FNatural too
   is
    CurHDUNum : Positive := 1;
    CurDUSize : FNatural;
    CurIndex  : SIO.Count := 0;
    OffsetInRootElem : SIO.Count;
    HDUSize   : HDU_Size_Type;
   begin

    SIO.Set_Index(FitsFile, 1);
    -- move to begining of the Primary HDU

    while CurHDUNum < HDUNum
    loop
     -- move past current Header
     Parse_Header(FitsFile, HDUSize);
     CurDUSize := Size_Blocks(HDUSize.DUSizeParam);
     -- move past DataUnit
     Move_Index_Behind_DataUnit(FitsFile,CurDUSize);
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
      CurIndex := SIO.Index(FitsFile);
      OffsetInRootElem := Count(Offset * DataType'Size / StreamElemSizeInBits );
      -- explicit conversion Natural -> Count ok: it is under if then... cannot be zero.
      -- FIXME But max values ? see Move_Index_...
      SIO.Set_Index(FitsFile, CurIndex + OffsetInRootElem);
    end if;

   end Set_Index;

   -- List size-related params of HDU
   procedure List_Content (FitsFile : in SIO.File_Type;
                           Print: not null access
                             procedure(HDUNum : Positive; HDUSize : HDU_Size_Type))
   is
    HDUCnt  : Positive := 1;
    HDUSize : HDU_Size_Type;
    CurDUSize : FNatural;
    CurIndex  : SIO.Count := 0;
   begin

    -- start from begining
    SIO.Set_Index(FitsFile,1);

    while not SIO.End_Of_File(FitsFile)
    loop
     -- read current DU-size
     Parse_Header(FitsFile, HDUSize);

     -- do the callback
     Print(HDUCnt,HDUSize);

     -- skip DataUnit
     CurDUSize := Size_Blocks(HDUSize.DUSizeParam);
     Move_Index_Behind_DataUnit(FitsFile,CurDUSize);

     -- read next HDU Header if any
     HDUCnt := HDUCnt + 1;
    end loop;

   end List_Content;

   -------------------------------------
   -------------------------------------
   -------------------------------------
   procedure Read (FitsStream : in SIO.Stream_Access;
                   Data       : in out DataArray_Type)
   is
   begin

        DataArray_Type'Read( FitsStream, Data );

   end Read;

   procedure Write (FitsStream : in SIO.Stream_Access;
                    Data       : in DataArray_Type)
   is
   begin

        DataArray_Type'Write( FitsStream, Data );

   end Write;

end FITS;

