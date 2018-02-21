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


   -- parse header in blocks
   --  from current file-index,
   --  read blocks until a block with END-card found
   --  and try to fill in HDUSize
   procedure oldParse_HeaderBlocks (FitsFile : in SIO.File_Type;
                                    HDUSize  : out HDU_Size_Type)
   is
    HBlk         : DataArray_Type(HBlock,1);
    Card         : Card_Type;
    ENDCardFound : Boolean := false;
    CardsCnt     : FNatural := 0;
    XtensionType : String(1..10) := (others => ' ');
   begin

    -- FIXME the below is not nice but relates to broader problem:
    -- how to make sure that each value needed for Size() calculations
    -- was set during parsing process (in Parse_Card/ParseHeaderBlock) ?
    HDUSize.DUSizeKeyVals.PCOUNT := 0;
    HDUSize.DUSizeKeyVals.GCOUNT := 1;
     -- init these for HDU's which do not use them
     -- BINTABLE and RandomGroup extensions, if present,
     -- will overwrite these values

    loop
      DataArray_Type'Read( SIO.Stream(FitsFile), HBlk );

      for I in HBlk.HBlockArr(1)'Range
      loop
        Card         := HBlk.HBlockArr(1)(I);
        Parse_Card(Card, HDUSize.DUSizeKeyVals);
        Parse_Card(Card, XtensionType);
        CardsCnt     := CardsCnt + 1;
        ENDCardFound := (Card = ENDCard);
        exit when ENDCardFound;
      end loop;

      exit when ENDCardFound;
    end loop;

    HDUSize.Xtension := XtensionType;
    HDUSize.CardsCnt := CardsCnt;
    -- here CardsCnt is > 0 otherwise
    -- we don't reach this line (leave by exception)

   end oldParse_HeaderBlocks;


   --
   -- Read File until ENDCard found
   --
   function Read_Header_Blocks
            (FitsFile : in SIO.File_Type;
             Data     : out Parsed_Type) return FPositive
   is
    HBlk         : DataArray_Type(HBlock,1);
    Card         : Card_Type;
    ENDCardFound : Boolean := false;
    CardsCnt     : FNatural := 0;
   begin

    -- FIXME how to make sure that each value of Parsed_Type
    -- was set during parsing process ?
    -- Parsed_Card implementation mus keep track of which
    -- record fields were set

    loop

      DataArray_Type'Read( SIO.Stream(FitsFile), HBlk );
      -- [FITS] every valid FITS File must have at least one block

      for I in HBlk.HBlockArr(1)'Range
      loop
        Card         := HBlk.HBlockArr(1)(I);
        Parse_Card(Card, Data);
        CardsCnt     := CardsCnt + 1;
        ENDCardFound := (Card = ENDCard);
        exit when ENDCardFound;
      end loop;

      exit when ENDCardFound;
    end loop;

    return CardsCnt;

   end Read_Header_Blocks;


   -- now try with generic

   procedure Parse_Card_For_Size
              (Card          : in  Card_Type;
               DUSizeKeyVals : out DU_Size_Type)
   is
   begin
    Parse_Card(Card, DUSizeKeyVals);
    DUSizeKeyVals.PCOUNT := 0;
    DUSizeKeyVals.GCOUNT := 1;
   end Parse_Card_For_Size;

   -- create Size-parsing func
   procedure Parse_HeaderBlocks (FitsFile : in  SIO.File_Type;
                                 HDUSize  : out HDU_Size_Type)
   is
    function ParseSizes is
      new Read_Header_Blocks (Parsed_Type => DU_Size_Type,
                              Parse_Card  => Parse_Card_For_Size);
   begin
     HDUSize.CardsCnt := ParseSizes(FitsFile, HDUSize.DUSizeKeyVals);
   end Parse_HeaderBlocks;

   -- END with GENERIC


   --
   -- Set file index to position given by params
   --
   procedure Set_Index (FitsFile : in SIO.File_Type;
                        HDUNum   : in Positive) -- to which HDU

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
     Parse_HeaderBlocks(FitsFile, HDUSize);

     -- do the callback
     Print(HDUCnt,HDUSize);

     -- skip DataUnit if exists
     if HDUSize.DUSizeKeyVals.NAXIS /= 0
     then
       CurDUSize_blocks := Size_blocks(HDUSize.DUSizeKeyVals);
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


end FITS.File;

