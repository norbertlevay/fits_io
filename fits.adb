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

   --
   -- convert BITPIX keyword from Header to internal FitsData_Type
   --
   function  To_FitsDataType (BITPIX : in Integer) return FitsData_Type
   is
    bp : FitsData_Type;
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
     -- FIXME raise exception "out of range"
     -- BITPIX is read from file, can be "whatever"
    end case;
    return bp;
   end To_FITSDataType;
   -- we need to separate BITPIX and FitsData_Type definition because
   -- Ada does not allow enumeration values to be negative (as needed for FloatNM)


   -- parse from Card value if it is one of DU_Size_Type, do nothing otherwise
   -- and store parse value to DUSizeKeyVals
   -- TODO what to do if NAXIS and NAXISnn do not match in a broken FITS-file
   -- [FITS,Sect 4.4.1.1]: NAXISn keys _must_ match NAXIS keyword.
   -- Size calc is valid also for IMAGE-extension, but not for TABLE extensions
   -- FIXME should check if it is IMAGE extension [FITS, Sect 7]
   procedure Parse_Card (Card          : in Card_Type;
                         DUSizeKeyVals : in out DU_Size_Type)
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
                         XtensionType : in out String)
   is
   begin
     if    (Card(1..9) = "XTENSION=") then
       XtensionType := Card(11..20);
     end if;
   end Parse_Card;

   -- reverse byte order for each FLoat32:
   --  4->1 3->2 2->3 1->4
   procedure Endianness_Float32( F32Arr : in out Float32Arr_Type )
   is
     type MyFloat is new Interfaces.IEEE_Float_32;
     type Arr4xU8 is array (1..4) of Interfaces.Unsigned_8;

     function MyFloat_To_Arr is
       new Ada.Unchecked_Conversion(Source => MyFloat, Target => Arr4xU8);
     function Arr_To_MyFloat is
       new Ada.Unchecked_Conversion(Source => Arr4xU8, Target => MyFloat);

     procedure SwapBytes(arr : in out Arr4xU8) is
      temp : Arr4xU8;
     begin
      temp(1) := arr(4);
      temp(2) := arr(3);
      temp(3) := arr(2);
      temp(4) := arr(1);
      arr := temp;
     end SwapBytes;

     aaaa : Arr4xU8;
   begin

     for I in F32Arr'Range
      loop

       aaaa := MyFloat_To_Arr(MyFloat(F32Arr(I)));
       SwapBytes(aaaa);
       F32Arr(I) := Interfaces.IEEE_Float_32(Arr_To_MyFloat(aaaa));

     end loop;

   end Endianness_Float32;



   procedure Float32Arr_Write
             (S      : access Ada.Streams.Root_Stream_Type'Class;
              F32Arr : in Float32Arr_Type)
   is
     type MyFloat is new Interfaces.IEEE_Float_32;
     type Arr4xU8 is array (1..4) of Interfaces.Unsigned_8;

     function MyFloat_To_Arr is
       new Ada.Unchecked_Conversion(Source => MyFloat, Target => Arr4xU8);
     function Arr_To_MyFloat is
       new Ada.Unchecked_Conversion(Source => Arr4xU8, Target => MyFloat);

     procedure SwapBytes(arr : in out Arr4xU8) is
      temp : Arr4xU8;
     begin
      temp(1) := arr(4);
      temp(2) := arr(3);
      temp(3) := arr(2);
      temp(4) := arr(1);
      arr := temp;
     end SwapBytes;

     aaaa : Arr4xU8;
     FBuff : MyFloat;
   begin

     if System.Default_Bit_Order = System.LOW_ORDER_FIRST
     then

       for I in F32Arr'Range
        loop
         aaaa := MyFloat_To_Arr(MyFloat(F32Arr(I)));
         SwapBytes(aaaa);
         FBuff := Arr_To_MyFloat(aaaa);
         MyFloat'Write(S, FBuff);
       end loop;

     else

       Float32Arr_Type'Write(S,F32Arr);

     end if;

   end Float32Arr_Write;


   -- find minimum and maximum value of the Float32 data array
   procedure Find_MinMax_Float32
              (F32Arr : in  Float32Arr_Type;
               Min    : out Interfaces.IEEE_Float_32;
               Max    : out Interfaces.IEEE_Float_32)
   is
     type MyFloat is new Interfaces.IEEE_Float_32;
   begin

     Min := Interfaces.IEEE_Float_32'Large;
     Max := Interfaces.IEEE_Float_32'Small;

     for D of F32Arr
      loop

       if D > Max then Max := D; end if;
       if D < Min then Min := D; end if;

     end loop;

   end Find_MinMax_Float32;



end FITS;

