
-- part of Header subsystem

with Header; -- Create_Mandatory needed

package body Elements is


   function Create_Card_SIMPLE(V : Boolean) return String_80_Array
   is
      Card : String_80_Array(1 .. 1) :=
         (1 => Header.Create_Mandatory_Card("SIMPLE", Header.To_Value_String(True)));
   begin
      return Card;
   end Create_Card_SIMPLE;


   function Create_Card_XTENSION(V : String) return String_80_Array
   is
      Card : String_80_Array(1 .. 1) :=
         (1 => Header.Create_Mandatory_Card("XTENSION", Header.To_Value_String(V)));
   begin
      return Card;
   end Create_Card_XTENSION;


   function Generate_Cards_Primary(Im : Image_Rec) return String_80_Array
   is
      Cards1 : String_80_Array := (
         Header.Create_Mandatory_Card("BITPIX",Header.To_Value_String(Image.BITPIX)),
         Header.Create_Mandatory_Card("NAXIS", Header.To_Value_String(Image.NAXISn'Length))
         );
      Cards : String_80_Array := Cards1 & Header.Create_NAXIS_Card_Arr(Image.NAXISn);
   begin
      return Cards;
   end Generate_Cards_Primary;


   function Generate_Cards_Extension
      (Im : Image_Rec;
      PCOUNT, GCOUNT : Positive_Count)
      return String_80_Array
   is
              Ext_Cards : String_80_Array := (
               Header.Create_Card("PCOUNT", Count'Image(PCOUNT)),
               Header.Create_Card("GCOUNT", Count'Image(GCOUNT))
               );
    begin
       return (Generate_Cards_Primary(Image) & Ext_Cards);
    end Generate_Cards_Extension;


   function Generate_Cards_Extension(Tab    : Table_Rec) return String_80_Array
   is
   begin
      return Generate_Cards_Extension(Tab.Image);
   end Generate_Cards_Extension;


   function Generate_Cards_Extension
      (BinTab : BinTable_Rec;
      GCOUNT : Positive_Count)
      return String_80_Array
   is
   begin
      return Generate_Cards_Extension(BinTab.Image, GCOUNT)
   end Generate_Cards_Extension;


end Elements;
