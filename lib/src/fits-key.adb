

package body FITS.Key is




  
    function Index
	   (CardKey : String;
	    RefKey  : Indexed_Key_Type) return Positive
    is
    begin
	    return 1;
    end Index;
	   







    function Compose
	    (Root  : String;
  	     Index : Positive) return String
    is
    begin
	    return "ComposedKEY";
    end Compose;


end FITS.Key;
