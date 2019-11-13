
-- implements FITSv3 Section 4.2 Fixed format variant

package Keyword_Record is

type Positive_Arr is array (Positive range <>) of Positive;
-- FIXME Formulas & FA_Prim/Ext need it; where to declare?


subtype Card_Type is String(1..80);
ENDCard  : constant Card_Type := (1 => 'E', 2 => 'N', 3 => 'D', others => ' ');

	
	function To_Boolean(Value : String) return Boolean;
	function To_Integer(Value : String) return Integer;
	function To_String (Value : String) return String;

	function Is_Natural(S : String) return Boolean;

	Invalid_Card_Value : exception;

end Keyword_Record;


-- not used:
--	function Is_ValuedCard (Card : Card_Type) return Boolean;

--	function To_String (Value : String) return String;
--	function To_Float  (Value : String) return Float;
--	function To_ComplexInteger(Value : String) return ???;
--	function To_ComplexFloat  (Value : String) return ???;
