

package FITS_IO is

   type Byte is mod 256;
   for Byte'Size use 8;
   -- [FITS] defines Byte as 8-bit


   CardSize : constant Positive := 80;
   -- [FITS Sects. 3.3.1, 4.4.1]

   subtype Card_Type is String(1..CardSize);
   -- makes sure index start with 1

   ENDCard   : constant Card_Type := ( 1=>'E', 2=>'N', 3=>'D', others => ' ');
   EmptyCard : constant Card_Type := (others => ' ');

   CardsCntInBlock : constant Positive := 36;
   type Card_Block is array (Positive range 1..CardsCntInBlock) of Card_Type;
   type Card_Arr   is array (Positive range <>)                 of Card_Type;
--   for Card_Arr'Size use Card_Arr'Length*(CardSize);
-- FIXME how to guarantee these Arrs are packed OR do we need to guarantee ?
   pragma Pack (Card_Block); -- not guaranteed ??
   pragma Pack (Card_Arr);   -- FIXME this is only suggestion to compiler

  -------------
  -- Numbers --
  -------------

   -- [FITS Sect 4.4.1.1] NAXISn is non-negative integer
   -- [FITS fixed integer]:
   -- Fixed integer is defined as 19 decimal digits
   -- (Header Card Integer value occupying columns 11..20)
   -- Lon_Long_Integer in GNAT is 64bit: 9.2 x 10**19 whereas
   -- fixed integer can reach 9.9 x 10**19)
   -- Conclude: range of NAXISn will be implementation
   -- limited as suggested in [FITS 4.2.3 Integer number]:
   type    Integer_64     is range -(2**63) .. +(2**63 - 1);
   -- 64bit portable: guaranteed to be 64bit or will not compile
   -- FIXME check against [FITS]: Why chosen 64bit:
   -- 64bit is almost as big as pathological case of
   -- card key-value of 20 charactes
   subtype Count          is Integer_64 range 0 .. Integer_64'Last;
   subtype Positive_Count is Count      range 1 .. Count'Last;

   subtype NAXIS_Type is Positive range 1 .. 999;
   -- [FITS, Sect 4.4.1]
   type NAXIS_Arr is array (NAXIS_Type range <>) of Positive_Count;





end FITS_IO;
