
-- high level block/HDU copying

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
-- with Ada.Unchecked_Conversion;
-- with Interfaces;

with Data_Block;
with Data_Funcs; use Data_Funcs;
with V3_Types;

package body File.Misc is

   type UInt8_Arr   is array ( Positive_Count range <> ) of V3_Types.Unsigned_8;
	-- use to write Padding

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
                           PadValue : in V3_Types.Unsigned_8)
   is  

    FillCnt   : constant Natural :=
       Natural( From rem SIO.Positive_Count(BlockSize_bytes) );
    PadLength : constant Natural :=
       Natural(BlockSize_bytes) - FillCnt + 1;


    PadArr    : constant UInt8_Arr(1 .. Positive_Count(PadLength)) := (others => PadValue);
    -- FIXME full of explicit casts!! review!!
   begin
    SIO.Set_Index(FitsFile,From);
    UInt8_Arr'Write(SIO.Stream(FitsFile),PadArr);
   end Write_Padding;


   -- Write Data by coordinates

   procedure To_Coords (Offset    : in  Positive_Count;
                        MaxCoords : in  Mandatory.NAXIS_Arr;
                        Coords    : out Mandatory.NAXIS_Arr)
   is
        use Mandatory;
      Sizes : Mandatory.NAXIS_Arr := MaxCoords;
      Divs :  Mandatory.NAXIS_Arr := MaxCoords;
      Rems :  Mandatory.NAXIS_Arr := MaxCoords;
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


  -- Write Data Unit

 procedure Write_Data_Unit (File : in SIO.File_Type;
                            DataElementCount : in SIO.Positive_Count)
 is

  package AnyType is new Data_Block(T => T);

  B : AnyType.Block;
  T_Size_bytes : constant Positive_Count := T'SIze / 8;  
  CountOfBlocks       : constant Positive_Count := DU_Block_Index(DataElementCount, T_Size_bytes);
  OffsetToLastElement : constant Positive := Offset_In_Block(DataElementCount, T_Size_bytes);
  OffInDU : Positive_Count := 1;
 begin

  -- write all except last block

  for NB in 1 .. (CountOfBlocks - 1)
  loop

    for O in 1 .. AnyType.N
    loop
      B(O) := Element(OffInDU);
      OffInDU := OffInDU + 1;
    end loop;

    AnyType.Block'Write(SIO.Stream(File), B); 

  end loop;

  -- write last block's data

  for O in 1 .. OffsetToLastElement
  loop
    B(O) := Element(OffInDU);
    OffInDU := OffInDU + 1;
  end loop;

  -- write padding

  for O in (OffsetToLastElement+1) .. AnyType.N
  loop
    B(O) := PadValue;

-- type-independent implementation commented out: PadValue generic param implementation 
-- prefered because 
-- A, it does not require Unchecked_Conversion 
-- and 
-- B, IEEE-float standard guarantees that bit pattern of +0.0 floats is always zeros which 
-- is the same as Pad-value in DataUnit in FITS
--
--    declare
--     Size_Bytes : constant Positive := T'Size / Interfaces.Unsigned_8'Size;
--     type ArrU8 is array (1..Size_Bytes) of Interfaces.Unsigned_8;
--     function Arr_To_Data is
--       new Ada.Unchecked_Conversion(Source => ArrU8, Target => T); 
--     Padding : ArrU8 := (others => 0);
--    begin
--      B(O) := Arr_To_Data(Padding);
--    end;

  end loop;

  AnyType.Block'Write(SIO.Stream(File), B);

 end Write_Data_Unit;









-- these two replace size calc funcs in .Misc subpackage Copy_HDU()
   function  DU_Size_blocks (FitsFile : in SIO.File_Type) return SIO.Count
   is  
        PSize : Mandatory.Result_Rec := Read_Header(FitsFile);
   begin
        return Calc_DataUnit_Size_blocks(PSize);
   end DU_Size_blocks;

  function  HDU_Size_blocks (FitsFile : in SIO.File_Type) return Positive
   is  
        PSize : Mandatory.Result_Rec := Read_Header(FitsFile);
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