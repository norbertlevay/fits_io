
-- part of Header subsystem

package Elements is

   subtype NAXIS_Index is Integer range 1 .. 999;
   type    NAXIS_Array is array (NAXIS_Index range <>) of Positive_Count;

   -- Image metadata

   type Image_Rec(NAXIS : NAXIS_Index) is
      record
         BITPIX : Integer;
         NAXISn : NAXIS_Array(1 .. NAXIS);
      end record;

   type Table_Rec(NAXIS : NAXIS_Index) is
      record
         BITPIX : Integer;
         NAXISn : NAXIS_Array(1 .. NAXIS);
      end record;

   type BinTable_Rec(NAXIS : NAXIS_Index) is
      record
         BITPIX : Integer;
         NAXISn : NAXIS_Array(1 .. NAXIS);
      end record;
 
   function Create_Card_SIMPLE(V : Boolean)  return String_80_Array;
   function Create_Card_XTENSION(V : String) return String_80_Array;
   -- FIXME V : String --> V : Bounded_20

   function Generate_Cards_Primary  (Im : Image_Rec) return String_80_Array;
   function Generate_Cards_Extension(Im : Image_Rec) return String_80_Array;
   function Generate_Cards_Extension(Tab    : Table_Rec)    return String_80_Array;
   function Generate_Cards_Extension(BinTab : BinTable_Rec) return String_80_Array;


end Elements;
