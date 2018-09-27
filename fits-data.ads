
-- FITS file's Data Unit

with FITS.File; use FITS.File;

package FITS.Data is

   procedure Find_MinMax_Float32
              (F32Arr : in  Float32_Arr;
               Min    : out Float_32;
               Max    : out Float_32);
   -- find minimum and maximum value of the Float32 data array

end FITS.Data;
