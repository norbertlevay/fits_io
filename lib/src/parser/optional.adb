

package body Optional is


function Init (Keys : in Bounded_String_8_Arr) return Positive
is
begin
	return 1;
end Init;


function Next (Pos  : in Positive; Card : in Card_Type) return Natural
is
begin
	return 1;
end Next;


function Get_Cards return Card_Arr
is
	Cards : Card_Arr(1..1) := ( others =>  (others => ' '));
begin
	return Cards;
end Get_Cards;

end Optional;
