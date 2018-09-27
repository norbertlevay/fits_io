
-- FITS file's Data Unit

with FITS.File; use FITS.File;

package FITS.Data is

   procedure Find_MinMax_Float32
              (F32Arr : in  Float32_Arr;
               Min    : out Float_32;
               Max    : out Float_32);
   -- find minimum and maximum value of the Float32 data array

   function Element(Data  : in UInt8_Arr;
                    Coord : in NAXIS_Arr) return Unsigned_8;

   function Element(Data  : in Float32_Arr;
                    Coord : in NAXIS_Arr) return Float_32;

end FITS.Data;
