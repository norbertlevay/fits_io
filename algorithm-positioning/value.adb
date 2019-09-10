
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

package body Value is

	function Is_ValuedCard (Card : Card_Type) return Boolean
	is 
	begin
		if(Card(9..10) = "= ") then
			return True;
		else
			return False;
		end if;

	end Is_ValuedCard;




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
			-- ERROR invalid value
			null;
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

	-- For Ada complex see: https://www.adaic.org/resources/add_content/standards/95lrm/ARM_HTML/RM-A-5.html
	-- with Ada.Numerics.Generic_Complex_Types;
  	-- package Complex_Types is new Ada.Numerics.Generic_Complex_Types (Long_Float);
   	-- package Complex_IO is new Ada.Text_IO.Complex_IO (Complex_Types);
--	function To_ComplexInteger(Value : String) return ???;
--	function To_ComplexFloat  (Value : String) return ???;







function Is_Array(Card : in  Card_Type;
                  Root : in  String;
                  First : in Positive;
                  Last  : in Positive;
                  Idx  : out Positive) return Boolean
is
        IsArray : Boolean := False;
        CardKey : String(1..8) := String(Card(1..8));
begin
        if(CardKey(1..Root'Length) = Root) then

                Idx := Positive'Value(CardKey(6..8));
                -- will raise exception if not convertible

                if ((Idx < First) OR (Idx > Last)) then
                        IsArray := False;
                else
                        IsArray := True;
                end if;

        end if;
        return IsArray;
end Is_Array;

end Value;
