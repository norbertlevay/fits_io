--
-- Implementation notes:
--
-- Set_Index allows positioning in the stream if the media allows it
-- (for files yes, for network maybe?, for pipes?, for stdin stdout?).
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

with Ada.Streams.Stream_IO;
use  Ada.Streams.Stream_IO;

package body FITS.File is

   procedure Move_Index
             (FitsFile : in SIO.File_Type;
              ByCount  : in SIO.Positive_Count) is
   begin
     SIO.Set_Index(FitsFile, SIO.Index(FitsFile) + ByCount);
   end Move_Index;
   pragma Inline (Move_Index);
   -- util: consider this part of Stream_IO

   ---------------
   -- FITS.File :

   StreamElemSize_bits : FPositive := Ada.Streams.Stream_Element'Size;
    -- FIXME [GNAT somwhere says it is 8bits]
    -- [GNAT]:
    --  type Stream_Element is mod 2 ** Standard'Storage_Unit;
    -- (Storage_Unit a.k.a 'Byte' : smallest addressable unit)
    -- note:
    --  type Count is new Stream_Element_Offset
    --                range 0 .. Stream_Element_Offset'Last;
    --  type Stream_Element_Offset is range
    --               -(2 ** (Standard'Address_Size - 1)) ..
    --               +(2 ** (Standard'Address_Size - 1)) - 1;
    -- Address_Size is 32 or 64bit nowadays

   BlockSize_bytes : FPositive := 2880*8 / StreamElemSize_bits;
   -- FIXME division : needs to be multiple of another otherwise
   --                  fraction lost
   -- in units of Stream_Element size (usually octet-byte)
   -- which is unit for positioning in Stream_IO by Set_Index() & Index()


   type Unit_Type is (HeaderUnit, DataUnit);

   -- from Data-type decide whether it is for Header or Data Unit
   --  arrays derived from Character are for Header Read/Write
   --  other (arrays derived from BITPIX) are for Data Unit access
   function  To_UnitType (DataType : in FitsData_Type) return Unit_Type
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

   -- parse one header for HDU-size information
   --  from current file-index,
   --  read cards until END-card found and try to fill in HDUSize
   procedure Parse_Header (FitsFile : in SIO.File_Type;
                           HDUSize  : in out HDU_Size_Type)
   is
    Card        : Card_Type;
    FreeSlotCnt : Natural;
   begin

    Card_Type'Read( SIO.Stream(FitsFile), Card );
    HDUSize.CardsCnt := 1;

    while Card /= ENDCard
    loop
      Parse_Card (Card, HDUSize.DUSizeParam);
      Card_Type'Read( SIO.Stream(FitsFile), Card );
      HDUSize.CardsCnt := HDUSize.CardsCnt + 1;
    end loop;

    -- FIXME should Parse_Header move upto next block limit ?
    -- there is nothing to parse after ENDCard -> procedure name is misleading
    -- Rather the move to next block-boundary should happen outside
    -- of Parse_Header by another explicit call.
    -- AND write other Parse_HeaderBlocks() which reads by
    -- HeaderBlocks instead of cards

    FreeSlotCnt := Free_Card_Slots(HDUSize.CardsCnt);
    -- read up to block-limit
    while FreeSlotCnt /= 0
    loop
      Card_Type'Read( SIO.Stream(FitsFile), Card );
      FreeSlotCnt := FreeSlotCnt - 1;
    end loop;

   end Parse_Header;

   --
   -- conversion FIXME review why needed ??
   --
   function  FitsDataTypeSize_bits(dt : FitsData_Type) return FNatural
   is
     Size : FNatural;
   begin
   case dt is
    when Int8 =>
     Size := Interfaces.Integer_8'Size;
    when Int16 =>
     Size := Interfaces.Integer_16'Size;
    when Int32 =>
     Size := Interfaces.Integer_32'Size;
    when Int64 =>
     Size := Interfaces.Integer_64'Size;
    when Float32 =>
     Size := Interfaces.IEEE_Float_32'Size;
    when Float64 =>
     Size := Interfaces.IEEE_Float_64'Size;
    when others =>
      null; -- FIXME exception or ?
    end case;
    return Size;
   end FITSDataTypeSize_bits;


   --
   -- Set file index to position given by params
   --
   procedure Set_Index (FitsFile : in SIO.File_Type;
                        HDUNum   : in Positive;      -- which HDU
                        DataType : in FitsData_Type := Card; -- decide to position to start of HeaderUnit or DataUnit
                        Offset   : in FNatural := 0)  -- offset within the Unit (in units of FitsData_Type)
   is
    CurDUSize_blocks : FPositive;
    CurDUSize_bytes  : FPositive;
    Offset_bytes     : FNatural;
    CurHDUNum : Positive := 1;
    HDUSize   : HDU_Size_Type;
    DataTypeSize_bits : FNatural := FitsDataTypeSize_bits(DataType);
   begin

    SIO.Set_Index(FitsFile, 1);
    -- move to begining of the Primary HDU

    while CurHDUNum < HDUNum
    loop
     -- move past current Header
     Parse_Header(FitsFile, HDUSize);

     -- skip DataUnit if exists
     if HDUSize.DUSizeParam.Naxes /= 0
     then
       CurDUSize_blocks := Size_blocks (HDUSize.DUSizeParam);
       CurDUSize_bytes  := CurDUSize_blocks * BlockSize_bytes;
       Move_Index(FitsFile, SIO.Positive_Count(CurDUSize_bytes));
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
     Offset_bytes := Offset * DataTypeSize_bits / StreamElemSize_bits;
      -- explicit conversions Natural -> Count ok:
      -- it is under if then... cannot be zero.
     Move_Index(FitsFile,SIO.Positive_Count(Offset_bytes));
    end if;

   end Set_Index;


   --
   -- List size-related params of HDU
   --
   procedure List_Content (FitsFile : in SIO.File_Type;
                           Print: not null access
                             procedure(HDUNum : Positive; HDUSize : HDU_Size_Type))
   is
    HDUCnt  : Positive := 1;
    HDUSize : HDU_Size_Type;
    CurDUSize_blocks : FPositive;
    CurDUSize_bytes  : FPositive;
    CurIndex  : SIO.Count := 0;
   begin

    -- start from begining
    SIO.Set_Index(FitsFile,1);

    while not SIO.End_Of_File(FitsFile)
    loop
     -- move past current Header
     Parse_Header(FitsFile, HDUSize);

     -- do the callback
     Print(HDUCnt,HDUSize);

     -- skip DataUnit if exists
     if HDUSize.DUSizeParam.Naxes /= 0
     then
       CurDUSize_blocks := Size_blocks(HDUSize.DUSizeParam);
       CurDUSize_bytes  := CurDUSize_blocks * BlockSize_bytes;
       Move_Index(FitsFile, SIO.Positive_Count(CurDUSize_bytes));
     end if;

     -- next HDU
     HDUCnt := HDUCnt + 1;
    end loop;

   end List_Content;


   -----------
   -- Utils --
   -----------

   --
   -- low-level copy in bigger chunks then one block for speed
   -- [FITS ???] suggests copying in chunks of 10 Blocks
   -- with today's machines 10 is probably outdated, but use 10 as default
   -- for every HW the optimal value will be different anyway
   --
   procedure Copy_Blocks (InFits  : in SIO.File_Type;
                          OutFits : in SIO.File_Type;
                          NBlocks : in FPositive;
                          ChunkSize_blocks : in Positive := 10)
   is
    NChunks   : FNatural := NBlocks  /  FPositive(ChunkSize_blocks);
    NRest     : FNatural := NBlocks rem FPositive(ChunkSize_blocks);
    BigBuf    : DataArray_Type(HBlock,ChunkSize_blocks); -- big buffer
    SmallBuf  : DataArray_Type(HBlock,1);                -- small buffer
   begin

     while NChunks > 0
     loop
      DataArray_Type'Read (SIO.Stream( InFits),BigBuf);
      DataArray_Type'Write(SIO.Stream(OutFits),BigBuf);
      NChunks := NChunks - 1;
     end loop;

     while NRest   > 0
     loop
      DataArray_Type'Read (SIO.Stream( InFits),SmallBuf);
      DataArray_Type'Write(SIO.Stream(OutFits),SmallBuf);
      NRest := NRest - 1;
     end loop;

   end Copy_Blocks;

   -- return size of the DU where InFits points to
   function  DU_Size_blocks (InFits  : in SIO.File_Type) return FNatural
   is
    HDUSizeRec : HDU_Size_Type;
   begin
    Parse_Header(InFits,HDUSizeRec);
    return Size_blocks(HDUSizeRec.DUSizeParam);
   end DU_Size_blocks;

   -- return size of the HDU where InFits points to
   function  HDU_Size_blocks (InFits  : in SIO.File_Type) return FNatural
   is
    HDUSizeRec : HDU_Size_Type;
   begin
    Parse_Header(InFits,HDUSizeRec);
    return Size_blocks(HDUSizeRec.CardsCnt) + Size_blocks(HDUSizeRec.DUSizeParam);
   end HDU_Size_blocks;

   --
   -- Copy all HDU
   --
   procedure Copy_HDU (InFits  : in SIO.File_Type;
                       OutFits : in SIO.File_Type;
                       HDUNum  : in Positive;
                       ChunkSize_blocks : in Positive := 10)
   is
     NBlocks : FPositive;
     HDUStartIdx : SIO.Positive_Count;
   begin
     HDUStartIdx := SIO.Index(InFits);
     -- calc size of HDU
     NBlocks := HDU_Size_blocks(InFits);
     -- go back to HDU-start & start copying...
     SIO.Set_Index(InFits, HDUStartIdx);
     Copy_Blocks(InFits,OutFits,NBlocks, ChunkSize_blocks);
   end Copy_HDU;


end FITS.File;

