
with Ada.Strings.Fixed;
--use Ada.Strings.Fixed;

with Keyword_Record; -- String_80 needed

package body Optional is

package ASF renames Ada.Strings.Fixed;


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

function Init (Keys : in Bounded_String_8_Arr) return SIO.Positive_Count
is
begin
    K_Last := Keys'Last;
    K(1..K_Last) := Keys;
    C_Last := 0;

    return 1;
end Init;


function Next (Pos  : in SIO.Positive_Count; Card : in String_80) return SIO.Count
is
    use SIO;
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



-- Valued-key record

-- NOTE for Valued_Key_Record later use tagged-record where Value string will
-- be replaced with Ada/FITS-numeric type and proper conversion function
-- numeric->String dispatched when read/write the card

 procedure VKR_Write (
            Stream : not null access Ada.Streams.Root_Stream_Type'Class;
            Item   : in  Valued_Key_Record)
 is
     Key    : String := BS_8.To_String(Item.Key);
     Value  : String := BS70.To_String(Item.Value);
     Key8   : String(1..8);--  := ASF.Trim( BS_8.To_String(Item.Key),   Ada.Strings.Left);
     Value20: String(1..20);-- := ASF.Trim( BS70.To_String(Item.Value), Ada.Strings.Right);
     EmptyComment : String(1..50) := (others => ' ');
     Card : String(1..80);-- := Key & " =" & Value & EmptyComment;
 begin
     ASF.Move(Key, Key8);
     ASF.Move(Value, Value20, Ada.Strings.Error, Ada.Strings.Right);
     Card := Key8 & "= " & Value20 & EmptyComment;
     String'Write(Stream, Card);
 end VKR_Write;


end Optional;
