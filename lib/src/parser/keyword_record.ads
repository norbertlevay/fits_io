
-- implements FITSv3 Section 4.2
--
-- 4.2.1 longest string is 68 chars: card(11..80) less the two quotes
-- 4.2.3 4.2.4 ..software packages can limit the range of Integer/float values...

package Keyword_Record is

subtype Card_Type is String(1..80);
ENDCard  : constant Card_Type := (1 => 'E', 2 => 'N', 3 => 'D', others => ' ');

        type    FInteger  is range -(2**63) .. +(2**63 - 1); 
        -- 64bit portable, guaranteed to be 64bit or will not compile
        subtype FNatural  is FInteger range 0 .. FInteger'Last;
        subtype FPositive is FNatural range 1 .. FNatural'Last;

	subtype FIndex is Integer range 0 .. 999;
	
	function To_Boolean(Value : String) return Boolean;
	function To_Integer(Value : String) return FInteger;
	function To_FIndex(Value : String) return FIndex;
	function To_String (Value : String) return String;
	

	function Match_Key(Key : in String; Card : in Card_Type) return Boolean;
	function Match_Indexed_Key(Root : in String; Card : in Card_Type) return Boolean;

	function Take_Index(Root : in String; Card : in Card_Type) return FIndex;

	Invalid_Card_Value : exception;

end Keyword_Record;


-- not used:
--	function Is_ValuedCard (Card : Card_Type) return Boolean;

--	function To_String (Value : String) return String;
--	function To_Float  (Value : String) return Float;
--	function To_Complex_Integer(Value : String) return ???;
--	function To_Complex_Float  (Value : String) return ???;
