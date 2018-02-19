--
-- Implementation notes:
--
-- FIXME make sure Ada Character type [Ada?][GNAT?]
-- is of same size as FITS Standard [FITS?] header-character

with Ada.Unchecked_Conversion;

with Interfaces;
use  Interfaces;

with System;
use  System;

with FITS.Header;
use  FITS.Header;

package body FITS is

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



   -- Endianness support

   generic
     type Data_Type is private;
   procedure Revert_Bytes( Data : in out Data_Type );

   procedure Revert_Bytes( Data : in out Data_Type )
   is
     Size_Bytes : Positive := Data_Type'Size / Byte'Size;
     type Arr4xU8 is array (1..Size_Bytes) of Interfaces.Unsigned_8;

     function Data_To_Arr is
       new Ada.Unchecked_Conversion(Source => Data_Type, Target => Arr4xU8);
     function Arr_To_Data is
       new Ada.Unchecked_Conversion(Source => Arr4xU8, Target => Data_Type);

     Arr  : Arr4xU8 := Data_To_Arr(Data);
     ArrO : Arr4xU8;
   begin

     for I in Arr'Range
     loop
       ArrO(I) := Arr(1 + Size_Bytes - I);
     end loop;

     Data := Arr_To_Data(ArrO);

   end Revert_Bytes;

   procedure Float32_Read_BigEndian
    		(S    : access Ada.Streams.Root_Stream_Type'Class;
             	 Data : out Float_32 )
   is
     procedure F32_Revert_Bytes is
       new Revert_Bytes( Data_Type => Float_32 );
   begin

     Interfaces.IEEE_Float_32'Read(S,Interfaces.IEEE_Float_32(Data));

     if System.Default_Bit_Order = System.LOW_ORDER_FIRST
     then
       F32_Revert_Bytes(Data);
     end if;

   end Float32_Read_BigEndian;

   procedure Float32_Write_BigEndian
    		(S    : access Ada.Streams.Root_Stream_Type'Class;
             	 Data : in Float_32 )
   is
     procedure F32_Revert_Bytes is
       new Revert_Bytes( Data_Type => Float_32 );
     DD : Float_32 := Float_32(Data);
   begin

     if System.Default_Bit_Order = System.LOW_ORDER_FIRST
     then
       F32_Revert_Bytes(DD);
     end if;

     Interfaces.IEEE_Float_32'Write(S,Interfaces.IEEE_Float_32(DD));

   end Float32_Write_BigEndian;


end FITS;

