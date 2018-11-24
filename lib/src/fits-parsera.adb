
with Ada.Strings.Bounded;
with Ada.Streams.Stream_IO;


package body FITS.ParserA is


   function Parse(Keys : in Parse_Key_Arr) return Key_Record_Arr
   is
    KeyRecs : Key_Record_Arr(1..2) ;
   begin
     -- Cards := Next; -- get next Card_Block from media (file, network, mem...)
     -- cycle throu all Cards in Card_Block,
     -- and compare them to Keys
     -- if match, fill in KeyRecs
     -- FIXME signal end if ENDCard found, How ?
     return KeyRecs;
   end Parse;


   function Parse_Mandatory return HDU_Size_Type
   is
    HDUSizeRec :  HDU_Size_Type ;
   begin
     return HDUSizeRec;
   end Parse_Mandatory;


end FITS.ParserA;
