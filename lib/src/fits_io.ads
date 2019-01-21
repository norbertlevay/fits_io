

package FITS_IO is

   type Byte is mod 256;
   for Byte'Size use 8;
   -- [FITS] defines Byte as 8-bit

   type    Integer_64     is range -(2**63) .. +(2**63 - 1);
   -- 64bit portable: guaranteed to be 64bit or will not compile
   subtype Count          is Integer_64 range 0 .. Integer_64'Last;
   subtype Positive_Count is Count      range 1 .. Count'Last;

   subtype NAXIS_Type is Positive range 1 .. 999;
   -- [FITS, Sect 4.4.1]
   type NAXIS_Arr is array (NAXIS_Type range <>) of Positive_Count;

end FITS_IO;
