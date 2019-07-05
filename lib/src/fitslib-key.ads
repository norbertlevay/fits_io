--
-- Decode/encode FITS special keyword types/forms like indexed keyword.
--
--
-- FIXME error/exception handling missing

package FITSlib.Key is

   type Indexed_Key_Type is
	   record
		   --Root : String(1..8);
		   Index_First : Natural;
		   Index_Last  : Positive;
	   end record;
   
    function Match
	   (CardKey : String;
	    RefRoot : String;
	    RefKey  : Indexed_Key_Type;
	    Index   : out Positive) return Boolean;
	    -- if False Index is undefined
	    
    function Compose
	    (Root  : String;
  	     Index : Positive) return String;


end FITSlib.Key;
