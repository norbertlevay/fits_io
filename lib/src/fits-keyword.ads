
with FITS.Header; use FITS.Header;


package FITS.Keyword is


   type Keyword_Type is tagged
    record
	Name    : Max_8.Bounded_String;
    end record;

    function Match(Key  : in Keyword_Type;
                   Card : in Card_Type) return Boolean;


   type Indexed_Keyword_Type is new Keyword_Type with
    record
        Index_First : Natural;
        Index_Last  : Positive;
        Index       : Natural;
    end record;

   function Match(Key  : in Indexed_Keyword_Type;
                  Card : in Card_Type) return Boolean;

end FITS.Keyword;

