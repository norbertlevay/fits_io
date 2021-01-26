


package body DU_Pos is


   function Get_ENDCard_Pos(P : Pos_Rec) return Positive_Count
   is begin return P.ENDCard_Pos; end Get_ENDCard_Pos;




   function Calc_DU_Item_Last
      (DU_Item_First : Positive_Count;
      Item_Len : Count) return Positive_Count
   is
      DU_Item_Last : Count;
   begin
      DU_Item_Last := DU_Item_First + Item_Len - 1;
      return DU_Item_Last;
   end Calc_DU_Item_Last;





   function Get_DU_Last(P : Pos_Rec) return Positive_Count
   is
   begin
      return P.DU_First + P.DU_Length - 1;
   end Get_DU_Last;




   function Calc_Chunk_Pos
      (DU_Current : Positive_Count;-- SIO.Positive_Count;
      DU_First  : Positive_Count;--SIO.Positive_Count;
      BITPIX    : Integer)
      return Positive_Count 
   is  
      N_First : Positive_Count;--FITS_IO.Positive_Count;
      SE_Size    : SIO.Positive_Count := Ada.Streams.Stream_Element'Size; -- bits
      --      use Ada.Streams.Stream_IO;
      T_Size_seu : Positive_Count := Positive_Count(abs BITPIX) / SE_Size;
      --T_Size_seu : SIO.Positive_Count := SIO.Positive_Count(abs BITPIX) / SE_Size;
      -- FIXME always divisable ?? -> platform param - seperate out to other ads
   begin
      N_First := Positive_Count (1 + (DU_Current - DU_First) / T_Size_seu);
      return N_First;
   end Calc_Chunk_Pos;



   function Is_Data_Padding_Written(Pos : Pos_Rec) return Boolean
   is
   begin
      return False;
   end Is_Data_Padding_Written;



end DU_Pos;
