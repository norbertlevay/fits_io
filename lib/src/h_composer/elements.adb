
-- part of Header subsystem

with Card;
with FITS_IO; use FITS_IO;

package body Elements is


   function Create_Card_SIMPLE(V : Boolean) return String_80_Array
   is
      Cards : String_80_Array(1 .. 1) :=
         (1 => Card.Create_Mandatory_Card("SIMPLE", Card.To_Value_String(True)));
   begin
      return Cards;
   end Create_Card_SIMPLE;


   function Create_Card_XTENSION(V : String) return String_80_Array
   is
      Cards : String_80_Array(1 .. 1) :=
         (1 => Card.Create_Mandatory_Card("XTENSION", Card.To_Value_String(V)));
   begin
      return Cards;
   end Create_Card_XTENSION;


   function Generate_Cards_Primary(Im : Image_Rec) return String_80_Array
   is
      Cards1 : String_80_Array := (
         Card.Create_Mandatory_Card("BITPIX",Card.To_Value_String(Im.BITPIX)),
         Card.Create_Mandatory_Card("NAXIS", Card.To_Value_String(Im.NAXISn'Length))
         );
      Cards : String_80_Array := Cards1 & Card.Create_NAXIS_Card_Arr(Im.NAXISn);
   begin
      return Cards;
   end Generate_Cards_Primary;


   function Generate_Cards_Extension
      (Im : Image_Rec;
      PCOUNT, GCOUNT : Positive_Count)
      return String_80_Array
   is
              Ext_Cards : String_80_Array := (
               Card.Create_Card("PCOUNT", Count'Image(PCOUNT)),
               Card.Create_Card("GCOUNT", Count'Image(GCOUNT))
               );
    begin
       return (Generate_Cards_Primary(Im) & Ext_Cards);
    end Generate_Cards_Extension;


   function Generate_Cards_Extension(Tab    : Table_Rec) return String_80_Array
   is
      Cards : String_80_Array(1..1);
   begin
      return Cards;--Generate_Cards_Extension(Image_Rec(Tab));
   end Generate_Cards_Extension;


   function Generate_Cards_Extension
      (BinTab : BinTable_Rec;
      GCOUNT : Positive_Count)
      return String_80_Array
   is
      Cards : String_80_Array(1..1);
   begin
      return Cards;--Generate_Cards_Extension(BinTab.Image, GCOUNT)
   end Generate_Cards_Extension;



   -- OO alternative


    function Generate_Cards(Image : Primary) return String_80_Array
    is  
       Cards1 : String_80_Array := (
            Card.Create_Mandatory_Card("BITPIX",Card.To_Value_String(Image.BITPIX)),
            Card.Create_Mandatory_Card("NAXIS", Card.To_Value_String(Image.NAXISn'Length))
            );
       Cards : String_80_Array := Cards1 & Card.Create_NAXIS_Card_Arr(Image.NAXISn);
    begin
       return Cards;
    end Generate_Cards;

    function Generate_Cards(Image : Conforming_Extension) return String_80_Array
    is  
        Ext_Cards : String_80_Array := (
               Card.Create_Card("PCOUNT", Count'Image(Image.PCOUNT)),
               Card.Create_Card("GCOUNT", Count'Image(Image.GCOUNT))
               );
    begin
       return (Generate_Cards(Primary(Image)) & Ext_Cards);
       -- FIXME how to call parent and avoid cast to Primary ?
    end Generate_Cards;




end Elements;
