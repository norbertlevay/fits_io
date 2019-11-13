
-- implementas FITSv3 Section 4.2


-- Value and comments share bytes 11..80
-- Value if present shall be: STRING LOGICAL or NUMERIC constant
-- Value maybe NULL (ALL SPACES) then keyword is UNDEFINED
-- If optional comment follows after value it MUST be preceded by '/'
-- space between value and '/' is strongly recommended

-- Type of keys is implicit: keyword name implies type
-- e.g. wno need to recognize type, we can parse directly for a given type

-- Value has 2 possible formats. strict (for Mandatory keys) and free (for other)

-- STRING rules:
-- enclosed: '       ' -> e.g. max length 68 chars (11..80 minus the opening closing quotes)
-- if ' needed in string it appears twice '' <-- ?!! What if ''''' ? 
-- leading spaces are significamt, trailing spaces not
-- Fixed format: 
-- -- byte 11 is ' and closing quote within/at byte80
-- -- special case XTENSION: 'IMAGE   '  'TABLE   '
-- Free format:
-- -- opening ' may start later then byte11, but must preceeded by spaces (downto byte11)

-- Special cases:
-- '' - null, zero length string
-- '    '  - empty string
-- =          - spaces (no quotes howver keys is string type) -> UNDEFINED keyword

-- Units are string values: appear as separate keywords or as part of comment: " / [unit]..."

-- LOGICAL: single character T|F
-- -- Fixed: byte30 = T|F
-- -- Free : from left, first non-space character (T|F) in 11..80

-- INTEGER:
-- Fixed: right justified in 11..30
-- Free : may appear anywhere between 11..80
-- is always signed decimal number
-- NOTE: 
-- "This standard does not limit the range of an integer keyword
-- value, however, software packages that read or write data ac-
-- cording to this standard could be limited in the range of values
-- that are supported (e.g., to the range that can be represented by
-- a 32-bit or 64-bit signed binary integer)."
 
-- NOTE for Mandatory keys implement Fixed Format definition parsing

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

package body Keyword_Record is

	function To_Boolean(Value : String) return Boolean
	is
		V : constant Character := Value(Value'First -1 + 30-10);
	begin
		case(V) is
			when 'T' =>
				return True;
			when 'F' =>
				return False;
			when others =>
				Raise_Exception(Invalid_Card_Value'Identity,
					"Expected Boolean but found " & Value);
		end case;

	end To_Boolean;


	function To_Integer(Value : String) return Integer
	is
	begin
		return Integer'Value(Value);
	end To_Integer;
	
	
	
	function To_String (Value : String) return String
	is
	begin
		-- separate optional comment from string value
		return Value;
	end To_String;



	function To_Float  (Value : String) return Float
	is
	begin
		return 0.0;
	end To_Float;

--	function To_ComplexInteger(Value : String) return ???;
--	function To_ComplexFloat  (Value : String) return ???;

	-- For Ada complex see: 
	-- https://www.adaic.org/resources/add_content/standards/95lrm/ARM_HTML/RM-A-5.html
	-- with Ada.Numerics.Generic_Complex_Types;
  	-- package Complex_Types is new Ada.Numerics.Generic_Complex_Types (Long_Float);
   	-- package Complex_IO is new Ada.Text_IO.Complex_IO (Complex_Types);


	function Is_Natural(S : String) return Boolean
	is 
  		Dummy : Natural;
	begin
	-- FIXME consider alternative implementation
	-- using Is_Digit(C) from Package: Characters.Handling

    		Dummy := Natural'Value (S);
	      	return True;
	   exception
	      when others =>
        	 return False;
	end Is_Natural;






-- ---------------------------------------------------------------------------
-- below not used
-- replaced with Is_Natural() and To_Integer()
-- as consequence we do not check for Index in First..Last as Is_Array did
-- FIXME should we ?


function Is_Array(Card : in  Card_Type;
                  Root : in  String;
                  First : in Positive;
                  Last  : in Positive;
                  Idx  : out Positive) return Boolean
is
        IsArray : Boolean := False;
        CardKey : String(1..8) := Card(1..8);
begin
        if(CardKey(1..Root'Length) = Root)
	then
                Idx := Positive'Value(CardKey(6..8));-- FIXME not 6 but Root'Length

		if ((Idx < First) OR (Idx > Last))
		then
                        IsArray := False;
                else
                        IsArray := True;
                end if;

	else
		IsArray := False;
		-- not Root-based array
        end if;

        return IsArray;

end Is_Array;


-- FIXME should this check array limits First..Last ? like for NAXISn : 1 <= Idx <= NAXIS_Val
function Extract_Index(Root : String; CardKey : String) return Positive
is
	RootLen : Positive := Root'Length;
begin
	return Positive'Value( CardKey( (CardKey'First+RootLen) .. (CardKey'First+7) ) );
end Extract_Index;







	function Is_ValuedCard (Card : Card_Type) return Boolean
	is 
	begin
		if(Card(9..10) = "= ") then
			return True;
		else
			return False;
		end if;

	end Is_ValuedCard;




end Keyword_Record;
