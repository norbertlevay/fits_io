
-- implementas FITSv3 Section 4.2 Fixed format variant

package Keyword_Record is
	
	function To_Boolean(Value : String) return Boolean;
	function To_Integer(Value : String) return Integer;

	function Is_Natural(S : String) return Boolean;

	Invalid_Card_Value : exception;

end Keyword_Record;


-- not used:
--	function Is_ValuedCard (Card : Card_Type) return Boolean;

--	function To_String (Value : String) return String;
--	function To_Float  (Value : String) return Float;
--	function To_ComplexInteger(Value : String) return ???;
--	function To_ComplexFloat  (Value : String) return ???;
