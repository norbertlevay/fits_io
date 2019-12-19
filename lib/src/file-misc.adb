
-- high level block/HDU copying

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;


package body File.Misc is

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


   -- Write Data by coordinates

   procedure To_Coords (Offset    : in  FPositive;
                        MaxCoords : in  Strict.Positive_Arr;
                        Coords    : out Strict.Positive_Arr)
   is
        use Strict;
      Sizes : Strict.Positive_Arr := MaxCoords;
      Divs :  Strict.Positive_Arr := MaxCoords;
      Rems :  Strict.Positive_Arr := MaxCoords;
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
                             MaxCoords : in  Strict.Positive_Arr)
   is

     function  multiply (MaxCoords : in  Strict.Positive_Arr) return FPositive
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
    Coord   : Strict.Positive_Arr := MaxCoords;

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


-- these two replace size calc funcs in .Misc subpackage Copy_HDU()
   function  DU_Size_blocks (FitsFile : in SIO.File_Type) return FNatural
   is  
        PSize : Strict.Result_Rec := Read_Header(FitsFile);
   begin
        return Calc_DataUnit_Size_blocks(PSize);
   end DU_Size_blocks;

  function  HDU_Size_blocks (FitsFile : in SIO.File_Type) return Positive
   is  
        PSize : Strict.Result_Rec := Read_Header(FitsFile);
   begin
        return Calc_HeaderUnit_Size_blocks(PSize.CardsCount)
               + Positive(Calc_DataUnit_Size_blocks(PSize)); -- FIXME down-conversion
   end HDU_Size_blocks;










   ------------------------------------
   -- Operations between two FITS-files
   ------------------------------------


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
     NBlocks := FPositive(HDU_Size_blocks(InFits)); -- FIXME explicit conversion
     -- go back to HDU-start & start copying...
     SIO.Set_Index(InFits, HDUStartIdx);
     Copy_Blocks(InFits,OutFits,NBlocks, ChunkSize_blocks);
   end Copy_HDU;


end File.Misc;
