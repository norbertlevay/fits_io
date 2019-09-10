
-- implementas FITSv3 Section 4.2

package Value is

	function To_String (Token : String) return String;
	function To_Boolean(Token : String) return Boolean;
	function To_Integer(Token : String) return Integer;
	function To_Float  (Token : String) return Double;

	-- For Ada complex see: https://www.adaic.org/resources/add_content/standards/95lrm/ARM_HTML/RM-A-5.html
	-- with Ada.Numerics.Generic_Complex_Types;
  	-- package Complex_Types is new Ada.Numerics.Generic_Complex_Types (Long_Float);
   	-- package Complex_IO is new Ada.Text_IO.Complex_IO (Complex_Types);
	function To_ComplexInteger(Token : String) return ???;
	function To_ComplexFloat  (Token : String) return ???;

end Values;
