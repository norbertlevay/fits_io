
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

-- FIXME SIO_DU_First can be calcd from SIO_ENDCard_Pos
-- both provide equivalent info but one used in Writes otehr in Reads


with Ada.Streams.Stream_IO;
with FITS; use FITS;
package DU_Pos is

  package SIO renames Ada.Streams.Stream_IO;
--  use SIO;

   type Pos_Rec is
      record
         -- Header (SIO Indexing from FITS file start)
         SIO_ENDCard_Pos    : SIO.Count;
         SIO_DU_First       : SIO.Count;
         -- Data (DU Indexing for Current DU: 1 .. DU_Length)
         DU_Length          : FITS.Count;
         DU_Padding_Written : Boolean;
      end record;
   -- SIO_ prefix: value is from all FITS begining in Stream_Elem count
   -- DU_ prefix : value is index to DU array from DU_First=1 of the current HDU

   Null_Pos_Rec : Pos_Rec := (0,0, 0, False); -- 0 meaning uninited

   -- events

   procedure Set_DU_Length  (Pos: in out Pos_Rec; L : Positive_Count);
   procedure Set_DU_First   (Pos: in out Pos_Rec; SIO_P : SIO.Positive_Count; BITPIX : Integer);
   procedure Set_ENDCard_Pos(Pos: in out Pos_Rec; SIO_P : SIO.Positive_Count);
   procedure Set_DU_Padding_Written(Pos: in out Pos_Rec; Written : Boolean);

   -- services

   function Get_ENDCard_Pos(P : Pos_Rec) return SIO.Positive_Count;
   function Get_DU_Last(P : Pos_Rec) return Positive_Count;

   function Is_Data_Padding_Written(Pos : Pos_Rec) return Boolean;

   -- util FIXME does this really belong here ?

   function DU_Index
      (SIO_Index : SIO.Positive_Count;
      SIO_DU_First : SIO.Positive_Count;
      BITPIX : Integer) return Positive_Count;

   function SE_Index
      (DU_Index  : Positive_Count;
      SIO_DU_First : SIO.Positive_Count;
      BITPIX : Integer) return SIO.Positive_Count;

   end DU_Pos;

