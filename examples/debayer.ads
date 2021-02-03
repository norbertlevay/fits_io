
with FITS;
with FITS_IO.V3_Types_For_DU;

package Debayer is

   package V3T renames FITS_IO.V3_Types_For_DU;

   subtype Count           is FITS.Count;
   subtype Positive_Count  is FITS.Positive_Count;
   subtype U8_Array        is V3T.U8_Arr;


   procedure Closest_Neighbour
      (Scan_Length : Positive_Count;
       Frame       : in out U8_Array);


end Debayer;

