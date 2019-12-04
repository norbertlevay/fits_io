
with Ada.Strings.Bounded; 	use Ada.Strings.Bounded;

package Optional is

package Bounded_String_8 is new Generic_Bounded_Length(8);

type Bounded_String_8_Arr  is array (Positive range <>) of Bounded_String_8.Bounded_String;


subtype Card_Type is String(1..80); -- temporary later use common Card_Type
type Card_Arr is array (Positive range <>) of Card_Type;

function Init (Keys : in Bounded_String_8_Arr) return Positive;
function Next (Pos  : in Positive; Card : in Card_Type) return Natural;
function Get_Cards return Card_Arr;

end Optional;
