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


with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Bounded;   use Ada.Strings.Bounded;

with FITS.Header; use FITS.Header;

package body FITS.File is

   BlockSize_bits : constant FPositive := 2880 * Byte'Size; -- 23040 bits
   -- [FITS 3.1 Overall file structure]

   type DU_Size_Type is record
      -- Primary HDU:
      BITPIX : Integer;     -- BITPIX from header (data size in bits)
      NAXIS  : NAXIS_Type;  -- NAXIS  from header
      NAXISn : NAXISn_Type;   -- NAXISn from header, 0 means dimension not in use
      -- Conforming extensions:
      PCOUNT : FNatural;    -- BINTABLE: size of heap OR Random Groups: param count preceding each group
      GCOUNT : FPositive;   -- Number of Random Groups present
      -- FIXME what type to use for P/GCOUNT ? -> implementation limited?
   end record;
   -- collects keyword values which define DataUnit size

   type HDU_Size_Type is record
      XTENSION      : String(1..10) := (others => '_'); -- XTENSION type string or empty [empty: FITS 4.2.1 undefined keyword]
      CardsCnt      : FPositive;     -- number of cards in this Header (gives Header-size)
      DUSizeKeyVals : DU_Size_Type;  -- keyword values to calc DataUnit-size
   end record;

   --
   -- calculate Header size in FITS Blocks
   --
   function  Size_blocks (CardsCnt    : in FPositive       ) return FPositive
   is
   begin
    return ( 1 + (CardsCnt - 1)/FPositive(CardsCntInBlock) );
   end Size_blocks;
   pragma Inline (Size_blocks);


   --
   -- calculate DataUnit size in FITS Blocks
   --
   -- implements Eq(1), (2) and (4) from [FITS]
   -- However we should parse other keys (SIMPLE, XTENSION, GROUPS) to
   -- establish HDU type - FIXME what strategy to take here ?
   function  Size_blocks (DUSizeKeyVals : in DU_Size_Type) return FPositive
   is
    DataInBlock    : FPositive;
    DUSizeInBlocks : FPositive;
    DUSize         : FPositive := 1;
    From : Positive := 1;
   begin

     -- if HDU is RandomGroup NAXIS1=0 and NAXIS1 is not part of size
     -- calculations [FITS Sect 6, Eq.(4)]
     if DUSizeKeyVals.NAXISn(1) = 0 then
      From := 2;
     end if;

     for I in From..DUSizeKeyVals.NAXIS
     loop
      DUSize := DUSize * DUSizeKeyVals.NAXISn(I);
     end loop;
      -- DUSize cannot be 0: Naxis(I) is FPositive
      -- cannot be 0 (parsing would throw exception)

     -- Conforming extensions (or 0 and 1 for Primary Header):
     DUSize := DUSize + DUSizeKeyVals.PCOUNT;
     DUSize := DUSize * DUSizeKeyVals.GCOUNT;

     DataInBlock := BlockSize_bits /  FNatural( abs DUSizeKeyVals.BITPIX );
     -- per FITS standard, these values are integer multiples (no remainder)

     DUSizeInBlocks := 1 + (DUSize - 1) / DataInBlock;

    return DUSizeInBlocks;
   end Size_blocks;
   pragma Inline (Size_blocks);


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


   -- parse from Card value if it is one of DU_Size_Type, do nothing otherwise
   -- and store parse value to DUSizeKeyVals
   -- TODO what to do if NAXIS and NAXISnn do not match in a broken FITS-file
   -- [FITS,Sect 4.4.1.1]: NAXISn keys _must_ match NAXIS keyword.
   -- Size calc is valid also for IMAGE-extension, but not for TABLE extensions
   -- FIXME should check if it is IMAGE extension [FITS, Sect 7]
   procedure Parse_Card (Card          : in Card_Type;
                         DUSizeKeyVals : out DU_Size_Type)
   is
    dim : Positive;
   begin
     -- FIXME what if parsed string is '' or '     ' etc...

     -- [FITS 4.1.2 Components]:
     -- pos 9..10 is '= '
     -- pos 31 is comment ' /'
     -- then : pos 10..20 is value

     if    (Card(1..9) = "BITPIX  =") then
       DUSizeKeyVals.BITPIX := Integer'Value(Card(10..30));

     elsif (Card(1..5) = "NAXIS") then

       if (Card(1..9) = "NAXIS   =") then
           DUSizeKeyVals.NAXIS := Positive'Value(Card(10..30));
       else
           dim := Positive'Value(Card(6..8));
           DUSizeKeyVals.NAXISn(dim) := FPositive'Value(Card(10..30));
           -- [FITS Sect 4.4.1.1] NAXISn is non-negative integer
           -- [FITS fixed integer]:
           -- Fixed integer is defined as 19 decimal digits
   	   -- (Header Card Integer value occupying columns 11..20)
   	   -- Lon_Long_Integer in GNAT is 64bit: 9.2 x 10**19 whereas
   	   -- fixed integer can reach 9.9 x 10**19)
           -- Conclude: range of NAXISn will be implementation
           -- limited as suggested in [FITS 4.2.3 Integer number]:
       end if;

     elsif (Card(1..5) = "PCOUNT") then
       DUSizeKeyVals.PCOUNT := FNatural'Value(Card(10..30));

     elsif (Card(1..5) = "GCOUNT") then
       DUSizeKeyVals.GCOUNT := FPositive'Value(Card(10..30));

     end if;

   end Parse_Card;

   procedure Parse_Card (Card         : in Card_Type;
                         XtensionType : out String)
   is
   begin
     if    (Card(1..9) = "XTENSION=") then
       XtensionType := Card(11..20);
     end if;
   end Parse_Card;

   procedure Parse_Card_For_Size
              (Card          : in  Card_Type;
               DUSizeKeyVals : out DU_Size_Type)
   is
   begin
    Parse_Card(Card, DUSizeKeyVals);
    DUSizeKeyVals.PCOUNT := 0;
    DUSizeKeyVals.GCOUNT := 1;
     -- init these for HDU's which do not use them
     -- BINTABLE and RandomGroup extensions, if present,
     -- will overwrite these values
   end Parse_Card_For_Size;



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
              CardsCnt : out FNatural;
              Xtension : out String);

   --
   -- Read File until ENDCard found
   --
   procedure  Read_Header_Blocks
             (FitsFile : in SIO.File_Type;
              Data     : out Parsed_Type;
              CardsCnt : out FNatural;
              Xtension : out String)
   is
    HBlk         : Card_Block;
    Card         : Card_Type;
    ENDCardFound : Boolean := false;
   begin

    -- FIXME how to make sure that each value of Parsed_Type
    -- was set during parsing process ?
    -- Parsed_Card implementation must keep track of which
    -- record fields were set

    CardsCnt := 0;

    loop

      Card_Block'Read( SIO.Stream(FitsFile), HBlk );
      -- [FITS] every valid FITS File must have at least one block

      for I in HBlk'Range
      loop
        Card         := HBlk(I);
        Parse_Card(Card, Xtension);
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
     ParseSizes(FitsFile, HDUSize.DUSizeKeyVals,
                HDUSize.CardsCnt, HDUSize.Xtension);
   end Parse_HeaderBlocks;

   -- the same as procedure above but as HDUSize struct generating function
   function Parse_HeaderBlocks (FitsFile : in  SIO.File_Type)
    return HDU_Size_Type
   is
    procedure ParseSizes is
      new Read_Header_Blocks (Parsed_Type => DU_Size_Type,
                              Parse_Card  => Parse_Card_For_Size);
    HDUSize : HDU_Size_Type;
   begin
     ParseSizes(FitsFile, HDUSize.DUSizeKeyVals,
                          HDUSize.CardsCnt,HDUSize.Xtension);
     return HDUSize;
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

   function Get (FitsFile : in  SIO.File_Type) return HDU_Info_Type
   is
    HDUSize : HDU_Size_Type := Parse_HeaderBlocks(FitsFile);
    HDUInfo : HDU_Info_Type(HDUSize.DUSizeKeyVals.NAXIS);
   begin
    HDUInfo.XTENSION := HDUSize.XTENSION;
    HDUInfo.CardsCnt := HDUSize.CardsCnt;
    HDUInfo.BITPIX   := HDUSize.DUSizeKeyVals.BITPIX;
    for I in HDUInfo.NAXISn'Range
      loop
        HDUInfo.NAXISn(I) := HDUSize.DUSizeKeyVals.NAXISn(I);
      end loop;
    return HDUInfo;
   end Get;

   -- Misc utils

   -- calc number of free cards to fill up HeaderBlock
   function  Free_Card_Slots (CardsCnt : in FPositive ) return Natural
   is
    FreeSlotCnt : Natural := Natural( CardsCnt mod FPositive(CardsCntInBlock) );
    -- explicit conversion ok: mod < CardsCntInBlock = 36;
   begin
    if FreeSlotCnt /= 0 then
      FreeSlotCnt := CardsCntInBlock - FreeSlotCnt;
    end if;
    return FreeSlotCnt;
   end Free_Card_Slots;
   pragma Inline (Free_Card_Slots);


   --
   -- convert BITPIX keyword from Header to internal Data_Type
   --
   function  To_DataType (BITPIX : in Integer) return Data_Type
   is
    bp : Data_Type;
   begin
    case BITPIX is
    when   8 => bp := UInt8;
    when  16 => bp := Int16;
    when  32 => bp := Int32;
    when  64 => bp := Int64;
    when -32 => bp := Float32;
    when -64 => bp := Float64;
    when others =>
     null;
     -- FIXME raise exception "out of range"
     -- BITPIX is read from file, can be "whatever"
    end case;
    return bp;
   end To_DataType;
   -- we need to separate BITPIX and FitsData_Type definition because
   -- Ada does not allow enumeration values to be negative (as needed for FloatNM)


end FITS.File;

