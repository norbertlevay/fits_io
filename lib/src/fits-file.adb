--
-- Implementation notes:
--
-- Set_Index allows positioning in the stream if the media allows it
-- (for files yes, for network maybe?, for pipes?, for stdin stdout?).
--
-- FIXME shouldn't FitsFile : File_Type be 'in out' ? we update the Index...
-- File_Type is Access. Nevertheless verify in out...
--
-- General API problem: which API call should move the file-index ?
--   And how to convey info where the new file-index points ?
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
--
--   Possible at all(?): 'Read/'Write are pre-defined Stream attributes with 2 param only(?)
--
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


with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Bounded;   use Ada.Strings.Bounded;

with Ada.Unchecked_Deallocation;

with FITS.Header; use FITS.Header;

with FITS.Parser;
with FITS.Parser.DUSize;

with FITS_IO.File;

package body FITS.File is

   -- Instantiate Parsers for File

   function Next_From_File(Source : in SIO.File_Type)
     return Card_Block
   is
   begin
    return Read_Cards(Source);
   end Next_From_File;

   package FP is new FITS.Parser(Source_Type => SIO.File_Type,
                                 Next   => Next_From_File);
   use FP;

   package FPDUSize is new FP.DUSize;
   use FPDUSize;


   -- Start FITS.File body

   BlockSize_bits : constant FPositive := 2880 * Byte'Size; -- 23040 bits
   -- [FITS 3.1 Overall file structure]

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

   BlockSize_bytes : FPositive := BlockSize_bits / StreamElemSize_bits;
   -- FIXME division : needs to be multiple of another otherwise
   --                  fraction lost
   -- in units of Stream_Element size (usually octet-byte)
   -- which is unit for positioning in Stream_IO by Set_Index()

   --
   -- Padding
   --
   -- Padding Data Unit: [FITS 3.3.2 Primary Data Array]
   -- If the data array does not fill the final data block, the remain-
   -- der of the data block shall be filled by setting all bits to zero.
   -- And for conforming Data Extensions [FITS 7.1.3]:
   -- The data format shall be identical to that of a primary data array
   -- as described in Sect. 3.3.2.
   procedure Write_Padding(FitsFile : in SIO.File_Type;
                           From     : in SIO.Positive_Count;
                           PadValue : in Unsigned_8)
   is

    FillCnt   : constant Natural :=
       Natural( From rem SIO.Positive_Count(BlockSize_bytes) );
    PadLength : constant Natural :=
       Natural(BlockSize_bytes) - FillCnt + 1;

    PadArr    : constant UInt8_Arr(1 .. FPositive(PadLength)) := (others => PadValue);
    -- FIXME full of explicit casts!! review!!
   begin
    SIO.Set_Index(FitsFile,From);
    UInt8_Arr'Write(SIO.Stream(FitsFile),PadArr);
   end Write_Padding;


   function  Read_Card  (FitsFile  : in  SIO.File_Type)
     return Card_Type
   is
     Card : Card_Type;
   begin
     Card_Type'Read(Stream(FitsFile),Card);
     return Card;
   end Read_Card;

   function  Read_Cards (FitsFile  : in  SIO.File_Type)
     return Card_Block
   is
     CardBlock : Card_Block;
   begin
     Card_Block'Read(Stream(FitsFile),CardBlock);
     return CardBlock;
   end Read_Cards;

   procedure Write_Card  (FitsFile : in SIO.File_Type;
                          Card     : in Card_Type)
   is
   begin
     Card_Type'Write(Stream(FitsFile),Card);
   end Write_Card;
   pragma Inline (Write_Card);

   procedure Write_Cards (FitsFile : in SIO.File_Type;
                          Cards    : in Card_Arr)
   is
   begin
     Card_Arr'Write(Stream(FitsFile),Cards);
   end Write_Cards;
   pragma Inline (Write_Cards);


   -- Read Data
   procedure gen_Read_Data (FitsFile : in  SIO.File_Type;
                            Data     : in out Data_Arr)
   is
   begin
     Data_Arr'Read(Stream(FitsFile),Data);
   end gen_Read_Data;
   pragma Inline (gen_Read_Data);

--   procedure Read_Data is new genRead_Data(UInt8_Arr);
--   procedure Read_Data is new genRead_Data(Int16_Arr);
--   procedure Read_Data is new genRead_Data(Int32_Arr);
--   procedure Read_Data is new genRead_Data(Int64_Arr);
--   procedure Read_Data is new genRead_Data(Float32_Arr);
--   procedure Read_Data is new genRead_Data(Float64_Arr);
--

--   procedure Read_Data (FitsFile : in  SIO.File_Type;
--                        Data     : in out UInt8_Arr)
--   is
--   begin
--     UInt8_Arr'Read(Stream(FitsFile),Data);
--   end Read_Data;
--   pragma Inline (Read_Data);

--   procedure Read_Data (FitsFile : in  SIO.File_Type;
--                        Data     : in out Float32_Arr)
--   is
--   begin
--     Float32_Arr'Read(Stream(FitsFile),Data);
--   end Read_Data;
--   pragma Inline (Read_Data);

   procedure gen_Write_Data (FitsFile : in SIO.File_Type;
                             Data     : in Data_Arr)
   is
   begin
     Data_Arr'Write(Stream(FitsFile),Data);
   end gen_Write_Data;
   pragma Inline (gen_Write_Data);


--   procedure Write_Data (FitsFile : in SIO.File_Type;
--                         Data     : in UInt8_Arr)
--   is
--   begin
--     UInt8_Arr'Write(Stream(FitsFile),Data);
--   end Write_Data;

   -- ... for all types ...

--   procedure Write_Data (FitsFile : in SIO.File_Type;
--                         Data     : in Float32_Arr)
--   is
--   begin
--     Float32_Arr'Write(Stream(FitsFile),Data);
--   end Write_Data;

--   procedure Write_Data (FitsFile : in SIO.File_Type;
--                         Data     : in Float64_Arr)
--   is
--   begin
--     Float64_Arr'Write(Stream(FitsFile),Data);
--   end Write_Data;

   -- REFACTOR for final

   procedure To_Coords (Offset    : in  FPositive;
                        MaxCoords : in  NAXIS_Arr;
                        Coords    : out NAXIS_Arr)
   is
      Sizes : NAXIS_Arr := MaxCoords;
      Divs :  NAXIS_Arr := MaxCoords;
      Rems :  NAXIS_Arr := MaxCoords;
      -- FIXME these inits are needed only to eliminate Ada error
      -- find other solution
   begin

    --
    -- generate size of each plane
    --
    declare
      Accu  : FPositive := 1;
    begin
      for I in MaxCoords'Range
      loop
       Accu := Accu * MaxCoords(I);
       Sizes(I) := Accu;
       -- FIXME Acc is not needed, init Sizes(1):=1 and use Sizes
      end loop;
    end;

    --
    -- calc divisions and fractions
    --
    declare
      PrevRem : FNatural := Offset - 1;
    begin
      for I in reverse MaxCoords'First .. MaxCoords'Last
      loop
        Divs(I) := 1 + PrevRem  /  Sizes(I);
        Rems(I) := 1 + PrevRem rem Sizes(I);
          -- FIXME rem gives 0 for multiples
        PrevRem := Rems(I) - 1;
      end loop;
    end;

    --
    -- pick the coordinates from Divs & Rems
    --
    Coords := Rems(Rems'First) & Divs(Rems'First..Divs'Last-1);
   end To_Coords;


   procedure Write_DataUnit (FitsFile  : in  SIO.File_Type;
                             MaxCoords : in  NAXIS_Arr)
   is

     function  multiply (MaxCoords : in  NAXIS_Arr) return FPositive
     is
      Accu  : FPositive := 1;
     begin
      for I in MaxCoords'Range
      loop
       Accu := Accu * MaxCoords(I);
      end loop;
      return Accu;
     end multiply;

    type Item_Arr is array (FPositive range <>) of Item;

    IArrLen : FPositive := multiply(MaxCoords);
    IArr    : Item_Arr(1..IArrLen);
    Coord   : NAXIS_Arr := MaxCoords;

    DPadCnt  : constant Positive  := 2880 - Natural(IArrLen mod FPositive(2880));
    ItemSize : constant Positive := Item'Size/Unsigned_8'Size;
    PadUInt8 : constant UInt8_Arr(1 .. FPositive(DPadCnt*ItemSize)) := (others => 0);
--    for Padding'Size use DPadCnt*(FITS.Data.Unsigned_8'Size);
-- FIXME How to guarantee that arrays are packed OR do we need to guarantee ?
   begin

    for I in IArr'Range
    loop
     To_Coords (I, MaxCoords, Coord);
     IArr(I) := Element (Coord);
    end loop;

    Item_Arr'Write(Ada.Streams.Stream_IO.Stream(FitsFile) ,IArr);
    UInt8_Arr'Write(Ada.Streams.Stream_IO.Stream(FitsFile) ,PadUInt8);

    -- FIXME write by Blocks

   end Write_DataUnit;





   --
   -- Read File until ENDCard found,
   -- cal Parse_Card for each card and
   -- return count of Cards
   --

   --
   -- Read File until ENDCard found
   --

    -- FIXME how to make sure that each value of Parsed_Type
    -- was set during parsing process ?
    -- Parsed_Card implementation must keep track of which
    -- record fields were set


   -- temporary converters from new parsing to old FITS-File code:

   -- convert DU_Size_Type -> HDU_Size_Type
   function Read_Header_And_Parse_Size(FitsFile : SIO.File_Type)
     return HDU_Size_Type
   is
     function OFFNext_From_File(Source : in SIO.File_Type)
       return Card_Block
     is
     begin
      return Read_Cards(Source);
     end OFFNext_From_File;
--     function Parse_HeadDUSize is
--          new FITS.Parser.DUSize.Parse_Header_For_DUSize(Source_Type => SIO.File_Type,
--                                                         Next        => Next_From_File);
--     DUSize  : DU_Size_Type  := Parse_HeadDUSize(FitsFile);
     DUSize  : DU_Size_Type  := Parse_Header_For_DUSize(FitsFile);
     HDUSize : HDU_Size_Type;
   begin
      HDUSize.CardsCnt := FPositive(DUSize.CardsCnt);-- FIXME explicit conversion
      HDUSize.BITPIX := DUSize.BITPIX;
      HDUSize.NAXIS  := DUSize.NAXISArr'Length;
      for I in DUSize.NAXISArr'Range
      loop
       HDUSize.NAXISn(I) := FNatural(DUSize.NAXISArr(I));
        -- FIXME explicit conversion Integer -> FInteger
      end loop;
     HDUSize.PCOUNT := 0;
     HDUSize.GCOUNT := 1;
     return HDUSize;
   end Read_Header_And_Parse_Size;

   -- convert DU_Size_Type -> HDU_Type
   function Read_Header_And_Parse_Type(FitsFile : SIO.File_Type)
     return HDU_Type
   is
     function OOONext_From_File(Source : in SIO.File_Type)
       return Card_Block
     is
     begin
      return Read_Cards(Source);
     end OOONext_From_File;
--     function Parse_HeadDUSize is
--          new FITS.Parser.DUSize.Parse_Header_For_DUSize(Source_Type => SIO.File_Type,
--                                                         Next        => Next_From_File);
--     DUSize  : DU_Size_Type  := Parse_HeadDUSize(FitsFile);
     DUSize  : DU_Size_Type  := Parse_Header_For_DUSize(FitsFile);
     HDUType : HDU_Type;
   begin
     HDUType.XTENSION := DUSize.XTENSION;
--     HDUType.XTENSION(1..DUSize.XTENSION'Length) := Max20.To_String(DUSize.XTENSION);
--     for I in DUSize.XTENSION'Range
--     loop
--       HDUType.XTENSION(I) := DUSize.XTENSION(I);
--     end loop;
     return HDUType;
   end Read_Header_And_Parse_Type;

   -- position to Header start before call with Set_Index(F,HDUNum)
   function  Get (FitsFile : in  SIO.File_Type) return HDU_Info_Type
   is
    HeaderStart : SIO.Positive_Count := Index(FitsFile);
--    HDUSize : HDU_Size_Type := Read_Header(FitsFile);
    HDUSize : HDU_Size_Type := Read_Header_And_Parse_Size(FitsFile);
    HDUInfo : HDU_Info_Type(HDUSize.NAXIS);
    HDUType : HDU_Type;
   begin
    Set_Index(FitsFile,HeaderStart);
--    HDUType := Read_Header(FitsFile);
    HDUType := Read_Header_And_Parse_Type(FitsFile);
    -- fill in returned struct
    HDUInfo.XTENSION := HDUType.XTENSION;
    HDUInfo.CardsCnt := HDUSize.CardsCnt;
    HDUInfo.BITPIX   := HDUSize.BITPIX;
    for I in HDUInfo.NAXISn'Range
      loop
        HDUInfo.NAXISn(I) := HDUSize.NAXISn(I);
      end loop;
    return HDUInfo;
   end Get;

   -- Old parsing
   --
   -- calculate DataUnit size in FITS Blocks
   --
   -- implements Eq(1), (2) and (4) from [FITS]
   -- However we should parse other keys (SIMPLE, XTENSION, GROUPS) to
   -- establish HDU type - FIXME what strategy to take here ?
   function  Size_blocks (HDUSize : in HDU_Size_Type) return FPositive
   is
    DataInBlock    : FPositive;
    DUSizeInBlocks : FPositive;
    DUSize         : FPositive := 1;
    From : Positive := 1;
   begin

     -- if HDU is RandomGroup NAXIS1=0 and NAXIS1 is not part of size
     -- calculations [FITS Sect 6, Eq.(4)]
     if HDUSize.NAXISn(1) = 0 then
      From := 2;
     end if;

     for I in From..HDUSize.NAXIS
     loop
      DUSize := DUSize * HDUSize.NAXISn(I);
     end loop;
      -- DUSize cannot be 0: Naxis(I) is FPositive
      -- cannot be 0 (parsing would throw exception)

     -- Conforming extensions (or 0 and 1 for Primary Header):
     DUSize := DUSize + HDUSize.PCOUNT;
     DUSize := DUSize * HDUSize.GCOUNT;

     DataInBlock := BlockSize_bits /  FNatural( abs HDUSize.BITPIX );
     -- per FITS standard, these values are integer multiples (no remainder)

     DUSizeInBlocks := 1 + (DUSize - 1) / DataInBlock;

    return DUSizeInBlocks;
   end Size_blocks;
   pragma Inline (Size_blocks);


   --
   -- Set file index to position given by params
   --
   procedure Set_Index(FitsFile : in SIO.File_Type;
                       HDUNum   : in Positive)
   is
    CurHDUSize_blocks : FPositive;
    CurHDUSize_bytes  : FPositive;
    CurHDUNum : Positive := 1;
    HDUSize   : HDU_Size_Type;

     procedure Move_Index
               (FitsFile : in SIO.File_Type;
                ByCount  : in SIO.Positive_Count) is
     begin
       SIO.Set_Index(FitsFile, SIO.Index(FitsFile) + ByCount);
     end Move_Index;
     pragma Inline (Move_Index);
     -- util: consider this part of Stream_IO
   begin
-- test FIXME begin
   FITS_IO.File.Set_Index(FitsFile,HDUNum);
   return;
-- test FIXME end

    SIO.Set_Index(FitsFile, 1);
    -- move to begining of the Primary HDU

    while CurHDUNum < HDUNum
    loop

     -- move past current Header
--     HDUSize := Read_Header(FitsFile);
     HDUSize := Read_Header_And_Parse_Size(FitsFile);
     -- skip DataUnit if exists
     if HDUSize.NAXIS /= 0
     then
       CurHDUSize_blocks := Size_blocks (HDUSize);
       CurHDUSize_bytes  := CurHDUSize_blocks * BlockSize_bytes;
       Move_Index(FitsFile, SIO.Positive_Count(CurHDUSize_bytes));
     end if;

     -- next HDU
     CurHDUNum := CurHDUNum + 1;
    end loop;

   end Set_Index;

   --
   -- calculate Header size in FITS Blocks
   --
   function  Size_blocks (CardsCnt    : in FPositive       ) return FPositive
   is
   begin
    return ( 1 + (CardsCnt - 1)/FPositive(CardsCntInBlock) );
   end Size_blocks;
   pragma Inline (Size_blocks);

   function  DU_Size (NAXISArr : in NAXIS_Arr) return FPositive
   is
    DUSize : FPositive := 1;
   begin
     for I in NAXISArr'Range
     loop
      DUSize := DUSize * NAXISArr(I);
     end loop;
     return DUSize;
   end DU_Size;

   -- return size of the DU where InFits points to
   function  DU_Size_blocks  (InFits  : in SIO.File_Type) return FNatural
   is
--    HDUSize : HDU_Size_Type := Read_Header(InFits);
    HDUSize : HDU_Size_Type := Read_Header_And_Parse_Size(InFits);
   begin
    return Size_blocks(HDUSize);
   end DU_Size_blocks;

   -- return size of the HDU where InFits points to
   function  HDU_Size_blocks (InFits  : in SIO.File_Type) return FNatural
   is
--    HDUSize : HDU_Size_Type := Read_Header(InFits);
    HDUSize : HDU_Size_Type := Read_Header_And_Parse_Size(InFits);
   begin
    return Size_blocks(HDUSize.CardsCnt) + Size_blocks(HDUSize);
   end HDU_Size_blocks;
-- FIXME both XX_Size_blocks funcs move file-Index and use Parse_HeaderBlocks:
-- by func-name Parse_Header should be enough to calc sizes.

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
    type CardBlock_Arr is array (1 .. ChunkSize_blocks) of Card_Block;
    BigBuf    : CardBlock_Arr; -- big buffer
    SmallBuf  : Card_Block;    -- small buffer
   begin

     while NChunks > 0
     loop
      CardBlock_Arr'Read (SIO.Stream( InFits),BigBuf);
      CardBlock_Arr'Write(SIO.Stream(OutFits),BigBuf);
      NChunks := NChunks - 1;
     end loop;

     while NRest   > 0
     loop
      Card_Block'Read (SIO.Stream( InFits),SmallBuf);
      Card_Block'Write(SIO.Stream(OutFits),SmallBuf);
      NRest := NRest - 1;
     end loop;

   end Copy_Blocks;

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

--   function To_Offset (Coords    : in  NAXIS_Arr;
--                       MaxCoords : in  NAXIS_Arr)
--     return FNatural
--   is
--    Offset : FNatural;
--    Sizes  : NAXIS_Arr := MaxCoords;
--   begin
--    if Coords'Length /= MaxCoords'Length
--    then
--     null;
--     -- raise exception <-- needed this if ?
--     -- no, check only high level inputs, this is not direct API call
--     -- assume if code corrct, it is corrct here
--    end if;
--
--    --
--    -- generate size of each plane
--    --
--    declare
--      Accu  : FPositive := 1;
--    begin
--      for I in MaxCoords'First .. (MaxCoords'Last - 1)
--      loop
--       Accu := Accu * MaxCoords(I);
--       Sizes(I) := Accu;
--       -- FIXME Acc is not needed, init Sizes(1):=1 and use Sizes
--      end loop;
--      end;
--
--    Offset := Coords(1) - 1;
--    for I in (Coords'First + 1) .. Coords'Last
--    loop
--     Offset := Offset + (Coords(I) - 1) * Sizes(I - 1);
--    end loop;
--
--    return Offset;
--   end To_Offset;

   -- not in use
--   procedure Set_Index_with_Offset(FitsFile : in SIO.File_Type;
--                       HDUNum   : in Positive;
--                       Coord    : in NAXIS_Arr := (1,1);
--                       MaxCoord : in NAXIS_Arr := (1,1);
--                       BITPIX   : in Positive  := 8)
--   is
--     SIO_Offset   : SIO.Positive_Count;
--     Offset       : FPositive;
--     SE_Size_bits : Positive := Ada.Streams.Stream_Element'Size;
--                                -- 'Size is in bits
--   begin
--
--     Set_Index(FitsFIle,HDUNum);
--     -- movef to beginig of HDU
--
--     -- next add offset up to Coord in array of BITPIX-type
--     Offset := To_Offset(Coord,MaxCoord);
--     if Offset > 0
--     then
--       SIO_Offset := SIO.Positive_Count(abs(BITPIX)/SE_Size_bits)
--                   * SIO.Positive_Count(Offset);
--       -- FIXME explicit conversions - verify!
--       Move_Index(FitsFile, SIO_Offset);
--     end if;
--
--   end Set_Index_with_Offset;

--   procedure Write_ENDCard(FitsFile : in SIO.File_Type)
--   is
--   begin
--     Card_Type'Write(Stream(FitsFile),ENDCard);
--     Write_Padding(FitsFile);
--   end Write_ENDCard;

--   procedure Write_Padding(FitsFile : in SIO.File_Type)
--   is
--    Pos      : constant SIO.Positive_Count := Index(FitsFile);
--    -- FIXME make sure CardSize and Pos are counted in
--    --       equal units (Stream ElemSize aka Bytes):
--    CardsCnt : constant SIO.Positive_Count :=
--               SIO.Positive_Count((Pos-1)/SIO.Positive_Count(CardSize));
--               -- distance from start of file in Card size
--    HPadCnt  : constant Positive := CardsCntInBlock -
--               Positive((CardsCnt) mod SIO.Positive_Count(CardsCntInBlock));
--    HPadArr  : constant Card_Arr(1 .. HPadCnt) := (others => EmptyCard);
--   begin
--     if HPadCnt /= CardsCntInBlock then
--        Card_Arr'Write(SIO.Stream(FitsFile),HPadArr);
--     end if;
--   end Write_Padding;
--


