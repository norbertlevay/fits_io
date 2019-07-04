--
-- Decode/encode FITS special keyword types/forms like indexed keyword.
--
--
-- FIXME error/exception handling missing

package FITS.Key is

   type Indexed_Key_Type is
	   record
		   Root : String(1..8);
		   Index_First : Natural;
		   Index_Last  : Positive;
	   end record;
   
    function Index
	   (CardKey : String;
	    RefKey  : Indexed_Key_Type) return Positive;
	    
    function Compose
	    (Root  : String;
  	     Index : Positive) return String;


end FITS.Key;
