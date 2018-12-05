
--with Ada.Containers.Doubly_Linked_Lists;
--with FITS.Header; use FITS.Header;
--with FITS.Keyword; use FITS.Keyword;
-- Child  can see all includs of Parent?

package FITS.Parser.DUSize is

   type NAXIS_Arr is array (Natural range <>) of Positive;

   type DU_Size_Type(NAXIS : Positive) is record
      XTENSION : Max20.Bounded_String;
      BITPIX   : Integer;
      NAXISArr : NAXIS_Arr(1..NAXIS);
   end record;



   function Naxis(ParsedKeys : in Out_Key_List.List)
     return Positive;
   -- return number of axis

   function To_DU_Size_Type(ParsedKeys : in Out_Key_List.List)
     return DU_Size_Type;


   generic
    type Source_Type is limited private;
    with function Next(Source : in Source_Type) return Card_Block;
   function Parse_Header_For_DUSize(Source : in Source_Type)
     return DU_Size_Type;

end FITS.Parser.DUSize;
