


package body DU_Pos is


   -- index conversions: SE Index -> DU Index -> SE Index

   function DU_Index(SIO_Index : Positive_Count; SIO_DU_First : Positive_Count; BITPIX : Integer)
      return Positive_Count
   is
      SE_Size : constant Positive_Count := Ada.Streams.Stream_Element'Size;
      DE_Size : constant Positive_Count := Positive_Count(abs BITPIX);
   begin
      return  1 + (SIO_Index - SIO_DU_First) / (DE_Size / SE_Size);
   end DU_Index;


   function SE_Index(DU_Index : Positive_Count; SIO_DU_First : Positive_Count; BITPIX : Integer)
      return Positive_Count
   is
      SE_Size : constant Positive_Count := Ada.Streams.Stream_Element'Size;
      DE_Size : constant Positive_Count := Positive_Count(abs BITPIX);
      use SIO;
      SIO_Index : SIO.Positive_Count
          := SIO_DU_First + SIO.Positive_Count( (DE_Size / SE_Size) * (DU_Index - 1));
   begin
      return SIO_Index;
   end SE_Index;
 

   -- Setters

   procedure Set_DU_Length  (Pos: in out Pos_Rec; L : Positive_Count)
   is
   begin
      Pos.DU_Length := L;
   end Set_DU_Length;

   procedure Set_DU_First   (Pos: in out Pos_Rec; SIO_P : Positive_Count; BITPIX : Integer)
   is
   begin
      Pos.SIO_DU_First := SIO_P;
   end Set_DU_First;


   procedure Set_ENDCard_Pos(Pos: in out Pos_Rec; SIO_P : Positive_Count)
   is
   begin
      Pos.SIO_ENDCard_Pos := SIO_P;
   end Set_ENDCard_Pos;

   procedure Set_DU_Padding_Written(Pos: in out Pos_Rec; Written : Boolean)
   is
   begin
      Pos.DU_Padding_Written := Written;
   end Set_DU_Padding_Written;

   function Get_ENDCard_Pos(P : Pos_Rec) return Positive_Count
   is
   begin
      return P.SIO_ENDCard_Pos;
   end Get_ENDCard_Pos;




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
      return P.DU_Length;
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
      return Pos.DU_Padding_Written;
   end Is_Data_Padding_Written;



end DU_Pos;
