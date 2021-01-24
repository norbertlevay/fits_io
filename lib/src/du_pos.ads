
-- purpose of this module:
-- * maintain ENDCard position to do Append_Cards (aka Write_Cards)
-- * maintain DU_Last to recognize when to write Data_Padding
-- and return Last param in HDU_Read and prevent to write over DU_Last in HDU_Write

-- FIXME DU_First DU_Last must be updated any time when ENDCard_Pos changes !!

generic
type T is range <>;
package DU_Pos is

   type Pos_Rec is
      record
         HDU_First   : T;        -- param FIXME should this be out of the package ??
--         BITPIX      : Integer;  -- fixed HDU property
         Data_Count  : T;        -- fixed HDU property : from NAXISn accumulated;

         --Card_Count  : T;              -- state : updated at each Writs_Cards
         DU_Padding_Written : Boolean; -- state : read by Close

         ENDCard_Pos : T;        -- cached : read at each Add_Cards -- needed only in Write
         DU_Last     : T;        -- cached : read at each Write_Data

         DU_First    : T;        -- cached : used locally in computations
      end record;

   -- events

   procedure Init(HDU_First : T; Data_Count : T) is null;

   procedure Set_HDU_First(P : T) is null;
   procedure Set_DU_First(P : T) is null;
   procedure Set_ENDCard_Pos(P : T) is null;
   -- also recalc DU_First DU_Last

   -- services

   -- for Header
   procedure Get_ENDCard_Pos(P : out T) is null;
   -- used by Write_Cards a.k.a. Append_Cards

   -- for Data_Unit

   -- FIXME add here: see Chunk_First Chunk_Last calcs and in HDU_Write -> Padding

   procedure Calc_DU_Last(DU_First : T;BITPIX : Integer;Data_Length : T; DU_Last : out T) is null;
   -- from ENDCard_Pos + HPadding + BITPIX Data_Length

   procedure Calc_DU_First(HDU_First : T; END_Card_Pos : T; DU_First : out T) is null;
   -- DU_First = ENDCard_Pos + HPad_length

   procedure Is_At_DU_Last(Chunk_First : T; Chunk_Len : T ; DPad_Written : Boolean) is null;
   -- based on :  Chunk_First <= DU_Last <= (Chunk_First + Chunk_Length)

   procedure Is_Data_Padding_Written(DPad_Exist : Boolean) is null;

end DU_Pos;
