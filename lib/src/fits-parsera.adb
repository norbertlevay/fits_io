
with Ada.Strings.Bounded;
with Ada.Streams.Stream_IO;


package body FITS.ParserA is


   function Parse(Keys : in Parse_Key_Arr) return Key_Record_Arr
   is
    KeyRecs : Key_Record_Arr(1..2) ;
   begin
     return KeyRecs;
   end Parse;



end FITS.ParserA;
