

generic
package FITS.Parser.DUSize is

   type NAXIS_Arr is array (Natural range <>) of Positive;

   type DU_Size_Type(NAXIS : Positive) is record
      CardsCnt : Positive;            -- FIXME these two do not
      XTENSION : Max20.Bounded_String;-- FIXME really belong here
      BITPIX   : Integer;
      NAXISArr : NAXIS_Arr(1..NAXIS);
   end record;



   function Naxis(ParsedKeys : in Out_Key_List.List)
     return Positive;
   -- return number of axis

   function To_DU_Size_Type(ParsedKeys : in Out_Key_List.List)
     return DU_Size_Type;


   function Parse_Header_For_DUSize(Source : in Source_Type)
     return DU_Size_Type;

end FITS.Parser.DUSize;
