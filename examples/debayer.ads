
with FITS;
with FITS_IO.V3_Types_For_DU;

package Debayer is

   package V3T renames FITS_IO.V3_Types_For_DU;
   subtype Positive_Count is FITS.Positive_Count;
   subtype U8_Array is V3T.U8_Arr;
use FITS;
   ScanLen : constant Positive_Count := 640*3;
   ScanCnt : constant Positive_Count := 513;

   use FITS;
   Frame_Size : constant FITS.Positive_Count := ScanLen * ScanCnt;
   --Frame : V3T.U8_Arr(1 .. Frame_Size);


   procedure Grey_8
      (Scan_Length : Positive_Count;
         Frame : in out U8_Array);


   procedure Closest_Neighbour
      (Scan_Length : Positive_Count;
         Frame : in out U8_Array);


end Debayer;
