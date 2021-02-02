
with FITS;
with FITS_IO.V3_Types_For_DU;

package Debayer is

   package V3T renames FITS_IO.V3_Types_For_DU;
   subtype Positive_Count is FITS.Positive_Count;
   subtype U8_Array is V3T.U8_Arr;

   ColsCnt : constant Positive_Count := 640; -- = Row_Length
   RowsCnt : constant Positive_Count := 513; -- = Column_Lenth
   Col_Length : constant Positive_Count := RowsCnt;

   use FITS;
   Frame_Size : constant FITS.Positive_Count := ColsCnt * RowsCnt;
   Frame : V3T.U8_Arr(1 .. Frame_Size);

   procedure Closest_Neighbour
      (Col_Length : Positive_Count;
         Frame : in out U8_Array);


end Debayer;
