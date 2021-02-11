
with FITS;
with V3_Types;

package Debayer is

   package V3T renames V3_Types;

   subtype Count           is FITS.Count;
   subtype Positive_Count  is FITS.Positive_Count;
   subtype U8_Array        is V3T.U8_Arr;


   procedure Closest_Neighbour
      (Scan_Length : Positive_Count;
       Frame       : in out U8_Array);


end Debayer;

