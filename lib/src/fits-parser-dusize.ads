
-- FIXME should be Mandatory - parsing al mandatory keywords as defd by standard

generic
   type Source_Type is private;
   with function Next(Source : Source_Type) return Card_Block is <>;
   -- source can be fits-file, memory (list of cards), network etc...
package FITS.Parser.DUSize is

   type NAXIS_Arr is array (Positive range <>) of Natural;

   type DU_Size_Type(NAXIS : Positive) is
   record
    BITPIX   : Positive;
    NAXISArr : NAXIS_Arr(1..NAXIS);
   end record;

   function Parse_DUSize(Source : in Source_Type)
     return DU_Size_Type;

end FITS.Parser.DUSize;
 