
-- high level block/HDU copying


with Ada.Streams.Stream_IO;
with Interfaces;

with Header; use Header;

with Keyword_Record; use Keyword_Record;-- String_80 needed

package body File.Misc is

   StreamElemSize_bits : Positive_Count := Ada.Streams.Stream_Element'Size;
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

   BlockSize_bytes : Positive_Count := BlockSize_bits / StreamElemSize_bits;
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
                           PadValue : in Interfaces.Unsigned_8)
   is  
    use SIO;
    FillCnt   : constant Natural :=
       Natural( From rem SIO.Positive_Count(BlockSize_bytes) );
    PadLength : constant Natural :=
       Natural(BlockSize_bytes) - FillCnt + 1;

    type UInt8_PadArr is array (SIO.Positive_Count range <>) of Interfaces.Unsigned_8;
    PadArr    : constant UInt8_PadArr(1 .. SIO.Positive_Count(PadLength)) := (others => PadValue);
    -- FIXME full of explicit casts!! review!!
   begin
    SIO.Set_Index(FitsFile,From);
    UInt8_PadArr'Write(SIO.Stream(FitsFile),PadArr);
   end Write_Padding;


   -- Write Data by coordinates

   procedure To_Coords (Offset    : in  Positive_Count;
                        MaxCoords : in  NAXIS_Arr;
                        Coords    : out NAXIS_Arr)
   is
        use Mandatory;
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
      Accu  : Positive_Count := 1;
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
      PrevRem : Count := Offset - 1;
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










-- these two replace size calc funcs in .Misc subpackage Copy_HDU()
   function  DU_Size_blocks (FitsFile : in SIO.File_Type) return Count
   is  
        PSize : Mandatory.Result_Rec := Read_Mandatory(FitsFile);
   begin
        return Calc_DataUnit_Size_blocks(PSize);
   end DU_Size_blocks;

  function  HDU_Size_blocks (FitsFile : in SIO.File_Type) return Positive_Count
   is  
        PSize : Mandatory.Result_Rec := Read_Mandatory(FitsFile);
   begin
        return Calc_HeaderUnit_Size_blocks(PSize.CardsCount)
               + Calc_DataUnit_Size_blocks(PSize);
   end HDU_Size_blocks;










   ------------------------------------
   -- Operations between two FITS-files
   ------------------------------------


    CardsCntInBlock : constant Positive := 36;
    -- FIXME Card_Block is duplication of def used internally inside Header.adb
    -- here use better other def, maybe from Data_Unit::UInt8 or Interfaces ??
    -- or simply based on FITS.Byte ? ==> Only size matters, we are not going to access
    -- data inside the block
    type Card_Block is array (Positive range 1..CardsCntInBlock) of String_80;
    pragma Pack (Card_Block);
    -- FIXME does Pack guarantee arr is packed? how to guarantee Arrs are packed$
    -- OR do we need to guarantee at all ?$





   -- low-level copy in bigger chunks then one block for speed
   -- [FITS ???] suggests copying in chunks of 10 Blocks
   -- with today's machines 10 is probably outdated, but use 10 as default
   -- for every HW the optimal value will be different anyway
   --
   procedure Copy_Blocks (InFits  : in SIO.File_Type;
                          OutFits : in SIO.File_Type;
                          NBlocks : in Positive_Count;
                          ChunkSize_blocks : in Positive := 10)
   is
    NChunks   : Count := NBlocks  /  Positive_Count(ChunkSize_blocks);
    NRest     : Count := NBlocks rem Positive_Count(ChunkSize_blocks);
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
     NBlocks : Positive_Count;
     HDUStartIdx : SIO.Positive_Count;
   begin
     HDUStartIdx := SIO.Index(InFits);
     -- calc size of HDU
     NBlocks := Positive_Count(HDU_Size_blocks(InFits)); -- FIXME explicit conversion
     -- go back to HDU-start & start copying...
     SIO.Set_Index(InFits, HDUStartIdx);
     Copy_Blocks(InFits,OutFits,NBlocks, ChunkSize_blocks);
   end Copy_HDU;


end File.Misc;
