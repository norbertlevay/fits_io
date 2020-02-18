


with Keyword_Record; -- String_80 needed

package body Optional is


-- Capacity (later replace with Lists or dynamic vectors)
Arr_Last  : constant Positive := 100;
-- max this many cards searched in one pass

K : Bounded_String_8_Arr(1 .. Arr_Last);
C : Card_Arr(1 .. Arr_Last);
-- positions in use in the above arrays
K_Last : Natural := 0; 
C_Last : Natural := 0;

-- FIXME everywhere fix array indexes: use 'First 'Last (loops and array asignments :=)
-- FIXME what happens if C-array remains empty (no key match) ? 
-- In Get_Cards: C(1..0) because C_Last remains 0

function Init (Keys : in Bounded_String_8_Arr) return Positive_Count
is
begin
    K_Last := Keys'Last;
    K(1..K_Last) := Keys;
    C_Last := 0;

    return 1;
end Init;


function Next (Pos  : in Positive_Count; Card : in String_80) return Count
is
begin
    if(Card = ENDCard) then return 0; end if;

    for I in 1 .. K_Last
    loop
        declare
            Key : String := Bounded_String_8.To_String(K(I));
        begin
            if(Card(1..Key'Last) = Key) 
            then
                C_Last := C_Last + 1;
                C(C_Last) := Card;
            end if;
        end;
    end loop;
    
    return Pos + 1;
end Next;


function Get_Cards return Card_Arr
is
    Cards : Card_Arr := C(1..C_Last);
begin
    return Cards;
end Get_Cards;



end Optional;
