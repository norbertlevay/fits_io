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


with Ada.Streams.Stream_IO;
use  Ada.Streams.Stream_IO;

with FITS.Header;
use  FITS.Header;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Bounded; use Ada.Strings.Bounded;

package body FITS.File is

   -- BEGIN newIF : some dummy funcs
   function To_Card (Key     : in Max_8.Bounded_String;
                     Value   : in Max20.Bounded_String;
                     Comment : in Max48.Bounded_String)
                     return Card_Type
   is
    Card : Card_Type := EmptyCard;
   begin
    -- FIXME how to guarantee Key and Comment are right justified
    --       Value (often) left justified
    Card(1 .. 8) := Max_8.To_String(Key);
    Card(9 ..10) := "= ";
    Card(11..30) := Max20.To_String(Value);
    Card(31..32) := " /"; -- [FITS 4.1.2.3: "Space strongly recommended" ]
    Card(33..80) := Max48.To_String(Comment);
    return Card;
   end To_Card;

   function To_Card (Key     : in Max_8.Bounded_String;
                     Comment : in Max70.Bounded_String)
                     return Card_Type
   is
    Card : Card_Type := EmptyCard;
   begin
    -- FIXME implement!
    return Card;
   end To_Card;

   -- should use generic
   function Element(Data  : in UInt8_Arr;
                    Coord : in Coord_Arr) return Unsigned_8
   is
    Elem : Unsigned_8 := 0;
   begin
    return Elem;
   end Element;

   function Element(Data  : in Float32_Arr;
                    Coord : in Coord_Arr) return Float_32
   is
    Elem : Float_32 := 0.0;
   begin
    return Elem;
   end Element;

 procedure To_Coords (Offset    : in  FPositive;
                      MaxCoords : in  Coord_Arr;
                      Coords    : out Coord_Arr)
 is
    Sizes : Coord_Arr := MaxCoords;
    Divs :  Coord_Arr := MaxCoords;
    Rems :  Coord_Arr := MaxCoords;
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

   function  multiply (MaxCoords : in  Coord_Arr) return FPositive
   is
    Accu  : FPositive := 1;
   begin
    for I in MaxCoords'Range
    loop
     Accu := Accu * MaxCoords(I);
    end loop;
    return Accu;
   end multiply;

   -- Padding Data Unit: [FITS 3.3.2 Primary Data Array]
   -- If the data array does not fill the final data block, the remain-
   -- der of the data block shall be filled by setting all bits to zero.
   -- And for conforming Data Extensions [FITS 7.1.3]:
   -- The data format shall be identical to that of a primary data array
   -- as described in Sect. 3.3.2.
   procedure Write_Data (FitsFile  : in  SIO.File_Type;
                         MaxCoords : in  Coord_Arr)
   is
    IArrLen : FPositive := multiply(MaxCoords);
    IArr    : Item_Arr(1..IArrLen);
--    for IArr'Size use IArrLen*(FITS.Data.Unsigned_8'Size);
    Coord   : Coord_Arr := MaxCoords;

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

   end Write_Data;

   -- END newIF


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

   BlockSize_bytes : FPositive := BlockSize_bits / StreamElemSize_bits;
   -- FIXME division : needs to be multiple of another otherwise
   --                  fraction lost
   -- in units of Stream_Element size (usually octet-byte)
   -- which is unit for positioning in Stream_IO by Set_Index()


   --
   -- Read File until ENDCard found,
   -- cal Parse_Card for each card and
   -- return count of Cards
   --
   generic
     type Parsed_Type is limited private;
     with procedure Parse_Card
                    (Card : in Card_Type;
                     Data : out Parsed_Type);
   procedure  Read_Header_Blocks
             (FitsFile : in SIO.File_Type;
              Data     : out Parsed_Type;
              CardsCnt : out FNatural);


   --
   -- Read File until ENDCard found
   --
   procedure  Read_Header_Blocks
             (FitsFile : in SIO.File_Type;
              Data     : out Parsed_Type;
              CardsCnt : out FNatural)
   is
    HBlk         : DataArray_Type(HBlock,1);
    Card         : Card_Type;
    ENDCardFound : Boolean := false;
--    CardsCnt     : FNatural := 0;
   begin

    -- FIXME how to make sure that each value of Parsed_Type
    -- was set during parsing process ?
    -- Parsed_Card implementation mus keep track of which
    -- record fields were set

    CardsCnt := 0;

    loop

      DataArray_Type'Read( SIO.Stream(FitsFile), HBlk );
      -- [FITS] every valid FITS File must have at least one block

      for I in HBlk.HBlockArr(1)'Range
      loop
        Card         := HBlk.HBlockArr(1)(I);
        Parse_Card(Card, Data); -- generic
        CardsCnt     := CardsCnt + 1;
        ENDCardFound := (Card = ENDCard);
        exit when ENDCardFound;
      end loop;

      exit when ENDCardFound;
    end loop;

   end Read_Header_Blocks;

   -- instantiate generic for Size parsing (see File.Size)

   procedure Parse_HeaderBlocks (FitsFile : in  SIO.File_Type;
                                 HDUSize  : out HDU_Size_Type)
   is
    procedure ParseSizes is
      new Read_Header_Blocks (Parsed_Type => DU_Size_Type,
                              Parse_Card  => Parse_Card_For_Size);
   begin
     ParseSizes(FitsFile, HDUSize.DUSizeKeyVals, HDUSize.CardsCnt);
   end Parse_HeaderBlocks;


   --
   -- Set file index to position given by params
   --
   procedure Set_Index_HDU (FitsFile  : in SIO.File_Type;
                            HDUNum    : in Positive)
   is
    CurDUSize_blocks : FPositive;
    CurDUSize_bytes  : FPositive;
    CurHDUNum : Positive := 1;
    HDUSize   : HDU_Size_Type;
   begin

    SIO.Set_Index(FitsFile, 1);
    -- move to begining of the Primary HDU

    while CurHDUNum < HDUNum
    loop
     -- move past current Header
     Parse_HeaderBlocks(FitsFile, HDUSize);

     -- skip DataUnit if exists
     if HDUSize.DUSizeKeyVals.NAXIS /= 0
     then
       CurDUSize_blocks := Size_blocks (HDUSize.DUSizeKeyVals);
       CurDUSize_bytes  := CurDUSize_blocks * BlockSize_bytes;
       Move_Index(FitsFile, SIO.Positive_Count(CurDUSize_bytes));
     end if;

     -- next HDU
     CurHDUNum := CurHDUNum + 1;
    end loop;

   end Set_Index_HDU;

   procedure Set_Index(FitsFile    : in SIO.File_Type;
                       HDUNum      : in Positive;
                       Coord       : in Coord_Arr := (1,1);
                       ElementType : in Element_Type := Char)
   is
   begin
     Set_Index_HDU(FitsFIle,HDUNum);

     -- FIXME add code to move FileIndex forward by Coord x ElementType

   end Set_Index;

   procedure Write_Padding(FitsFile : in SIO.File_Type)
   is
    Pos      : constant SIO.Positive_Count := Index(FitsFile);
    -- FIXME make sure CardSize and Pos are counted in
    --       equal units (Stream ElemSize aka Bytes):
    CardsCnt : constant SIO.Positive_Count :=
               SIO.Positive_Count((Pos-1)/SIO.Positive_Count(CardSize));
               -- distance from start of file in Card size
    HPadCnt  : constant Positive := CardsCntInBlock -
               Positive((CardsCnt) mod SIO.Positive_Count(CardsCntInBlock));
    HPadArr  : constant Card_Arr(1 .. HPadCnt) := (others => EmptyCard);
   begin
     if HPadCnt /= CardsCntInBlock then
        Card_Arr'Write(SIO.Stream(FitsFile),HPadArr);
     end if;
   end Write_Padding;

   procedure Write_ENDCard(FitsFile : in SIO.File_Type)
   is
   begin
     Card_Type'Write(Stream(FitsFile),ENDCard);
     Write_Padding(FitsFile);
   end Write_ENDCard;

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
    Parse_HeaderBlocks(InFits,HDUSizeRec);
    return Size_blocks(HDUSizeRec.DUSizeKeyVals);
   end DU_Size_blocks;

   -- return size of the HDU where InFits points to
   function  HDU_Size_blocks (InFits  : in SIO.File_Type) return FNatural
   is
    HDUSizeRec : HDU_Size_Type;
   begin
    Parse_HeaderBlocks(InFits,HDUSizeRec);
    return Size_blocks(HDUSizeRec.CardsCnt) + Size_blocks(HDUSizeRec.DUSizeKeyVals);
   end HDU_Size_blocks;
-- FIXME both XX_Size_blocks funcs move file-Index and use Parse_HeaderBlocks:
-- by func-name Parse_Header should be enough to calc sizes.


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


   -- New IF
   procedure Get (FitsFile : in  SIO.File_Type;
                  HDUInfo  : out HDU_Info_Type)
   is
    HDUSize : HDU_Size_Type;
   begin
    Parse_HeaderBlocks(FitsFile, HDUSize);

    HDUInfo.XTENSION := HDUSize.XTENSION;
    HDUInfo.CardsCnt := HDUSize.CardsCnt;
    HDUInfo.BITPIX   := HDUSize.DUSizeKeyVals.BITPIX;
    HDUInfo.NAXIS    := HDUSize.DUSizeKeyVals.NAXIS;
    HDUInfo.NAXISn   := HDUSize.DUSizeKeyVals.NAXISn;
   end Get;

end FITS.File;

