

with FITS_IO; use FITS_IO;

generic
type T is range <>;
package DU_Pos is

   type Pos is
      record
         HDU_First   : T;        -- param
         BITPIX      : Integer;  -- fixed HDU property
         Data_Count  : T;        -- fixed HDU property : from NAXISn accumulated;

         Card_Count  : T;              -- state : updated at each Writs_Cards
         DU_Padding_Written : Boolean; -- state : read by Close

         ENDCard_Pos : T;        -- cached : read at each Add_Cards
         DU_Last     : T;        -- cached : read at each Write_Data

         DU_First    : T;        -- cached : used locally in computations
      end record;


   procedure Init(HDU_First : T; BITPIX : Integer; Data_Count : T) is null;

   procedure Init_Cards_Count(NAXIS : NAXIS_Index; N : out T) is null;
   procedure Set_ENDCard_Pos(P : T) is null;
   procedure Get_ENDCard_Pos(P : out T) is null;

   procedure Update_Cards_Count(Cards_Appended : Positive_Count; N : out T) is null;
   -- also recalc DU_First DU_Last

   procedure Calc_DU_Last(DU_First : T;BITPIX : Integer;Data_Length : T; DU_Last : out T) is null;
   -- from ENDCard_Pos + HPadding + BITPIX Data_Length

   procedure Calc_DU_First(HDU_First : T; END_Card_Pos : T; DU_First : out T) is null;
   -- DU_First = ENDCard_Pos + HPad_length

   procedure Is_DPadding_Written(Chunk_First : T; Chunk_Len : T ; DPad_Written : Boolean) is null;
   -- based on :  Chunk_First <= DU_Last <= (Chunk_First + Chunk_Length)

end DU_Pos;
