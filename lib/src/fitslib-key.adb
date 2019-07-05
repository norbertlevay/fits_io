-- [FITS] p4. Glossary
--
-- Indexed keyword. A keyword name that is of the form of a
-- fixed root with an appended positive integer index number.
--
-- Keyword name. The first eight bytes of a keyword record
-- which contain the ASCII name of a metadata quantity (un-
-- less it is blank).
-- 
-- Keyword record. An 80-character record in a header block
-- consisting of a keyword name in the first eight characters
-- followed by an optional value indicator, value and comment
-- string. The keyword record shall be composed only of the re-
-- stricted set of ASCII text characters ranging from decimal 32
-- to 126 (hexadecimal 20 to 7E).
--
-- [FITS] p-7
-- For indexed keyword names that have a single positive integer 
-- index counter appended to the root name, the counter shall 
-- not have leading zeroes (e.g.,NAXIS1, not NAXIS001). 
-- Note that keyword names that begin with (or consist solely of) 
-- any combination of hyphens, under-scores, and digits are legal.
--

package body FITSlib.Key is


    function Match
           (CardKey : String;
            RefRoot : String;
            RefKey  : Indexed_Key_Type;
            Index   : out Positive) return Boolean
    is
	    ParsedIndex : Positive;
	    Root      : String  := RefRoot;
	    Match     : Boolean := False;
	    HasIndex  : Boolean := Root'Length < CardKey'Length;
	    CardKeyRoot_Last : Positive;
	    RootMatch : Boolean;
    begin
	    
	    if (NOT HasIndex)
	    then
		    return False;
	    end if;

	    CardKeyRoot_Last := CardKey'First + Root'Length - 1;
	    RootMatch := (Root = CardKey(CardKey'First..CardKeyRoot_Last));
	
	    if (NOT RootMatch)
	    then
		    return False;
	    end if;
	    
	    begin
		    ParsedIndex := Natural'Value(CardKey(CardKeyRoot_Last+1 .. CardKey'Last));
	    exception
	    -- conversion attempt failed, not a number
		    when ExceptID : others =>
		    return False;
	    end; 
	    
	    Match := ( (RefKey.Index_First <= ParsedIndex) AND 
		       (RefKey.Index_Last  >= ParsedIndex) );

	    if (Match) 
	    then 
		    Index := ParsedIndex;
	    end if;

	    return Match;

    end Match;
	





    function Compose
	    (Root  : String;
  	     Index : Positive) return String
    is
    begin
	    return "ComposedKEY";
    end Compose;


end FITSlib.Key;
