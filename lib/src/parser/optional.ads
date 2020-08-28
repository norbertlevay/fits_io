-- TODO:
-- implement Optional for IndexedKeys (arrays)

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- (Positive_)Count needed

with Ada.Strings.Bounded; 	use Ada.Strings.Bounded;

with Keyword_Record; use Keyword_Record; -- String_80 needed

package Optional is

package Bounded_String_8 is new Generic_Bounded_Length(8);

type Bounded_String_8_Arr  is array (Positive range <>) of Bounded_String_8.Bounded_String;


type Card_Arr is array (Positive range <>) of String_80;
-- FIXME consider Index of Card_Arr derived from NAXIS value type (SIO.Positive)
-- theory in can be as big as DataUnit-length/80 
Null_Card_Arr : Optional.Card_Arr(1 .. 0) := (others => ENDCard);

function Init (Keys : in Bounded_String_8_Arr) return Positive_Count;
function Next (Pos  : in Positive_Count; Card : in String_80) return Count;
function Get_Cards return Card_Arr;

end Optional;
