
with Ada.Strings.Bounded;
with Ada.Streams.Stream_IO;


package body FITS.ParserA is


   function Parse(Keys : in Parse_Key_Arr) return Key_Record_Arr
   is
    KeyRecs : Key_Record_Arr(1..2) ;
   begin
     return KeyRecs;
   end Parse;


   function Parse_Mandatory return HDU_Size_Type
   is
    HDUSizeRec :  HDU_Size_Type ;
   begin
     return HDUSizeRec;
   end Parse_Mandatory;


end FITS.ParserA;
