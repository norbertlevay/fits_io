
-- purpose of this module:
-- * maintain ENDCard position to do Append_Cards (aka Write_Cards)
-- * maintain DU_Last to recognize when to write Data_Padding
-- and return Last param in HDU_Read and prevent to write over DU_Last in HDU_Write

-- FIXME DU_First DU_Last must be updated any time when ENDCard_Pos changes !!

-- NOTE Pos_Rec holds:
-- end of Header: ENDCard_Pos
-- end of Data U: DU_Last = DU_First + DU_Length - 1
-- NOTE actually as DU_Length is known -> DU_First & DU_Last is calculable
-- FIXME --> enough to store ENDCard_Pos and const DU_Length ?

-- FIXME DU_First can be calcd from ENDCard_Pos
-- both provide equivalent info but one used in Writes otehr in Reads


with Ada.Streams.Stream_IO;

package DU_Pos is

   package SIO renames Ada.Streams.Stream_IO;
   use SIO;

   type Pos_Rec is
      record
         DU_Length   : Count;
         ENDCard_Pos : Count; -- state: used in Writes
         DU_First    : Count; -- calculable: =func(ENDCard_Pos, BlkSite): used in Reads 
         DU_Padding_Written : Boolean; -- state : read by Close
      end record;

   Null_Pos_Rec : Pos_Rec := (0,0,0 ,False); -- 0 meaning uninited

   -- events

   procedure Set_DU_Length  (Pos: in out Pos_Rec; BITPIX : Integer; L : Positive_Count) is null;
   procedure Set_DU_First   (Pos: in out Pos_Rec; P : Positive_Count) is null;
   procedure Set_ENDCard_Pos(Pos: in out Pos_Rec; P : Positive_Count) is null;

   -- services

   function Get_ENDCard_Pos(P : Pos_Rec) return Positive_Count;
   function Get_DU_Last(P : Pos_Rec) return Positive_Count;

   function Calc_DU_Item_Last
      (DU_Item_First : Positive_Count;
      Item_Len : Count) return Positive_Count;

   function Is_Data_Padding_Written(Pos : Pos_Rec) return Boolean;

   end DU_Pos;

