
-- implementas FITSv3 Section 4.2 Fixed format variant
with Primary_Size_Info; -- Card_Type needed
use  Primary_Size_Info; -- Card_Type needed

package Value is
	
	function Is_ValuedCard (Card : Card_Type) return Boolean;

	function To_String (Value : String) return String;
	function To_Boolean(Value : String) return Boolean;
	function To_Integer(Value : String) return Integer;
	function To_Float  (Value : String) return Float;

	-- For Ada complex see: https://www.adaic.org/resources/add_content/standards/95lrm/ARM_HTML/RM-A-5.html
	-- with Ada.Numerics.Generic_Complex_Types;
  	-- package Complex_Types is new Ada.Numerics.Generic_Complex_Types (Long_Float);
   	-- package Complex_IO is new Ada.Text_IO.Complex_IO (Complex_Types);
--	function To_ComplexInteger(Value : String) return ???;
--	function To_ComplexFloat  (Value : String) return ???;


	-- temporarile put here
	function Is_Array(Card : in  Card_Type;
                  Root : in  String;
                  First : in Positive;
                  Last  : in Positive;
                  Idx  : out Positive) return Boolean;
 
end Value;
