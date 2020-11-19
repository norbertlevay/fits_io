
with Ada.Strings.Fixed;
--use Ada.Strings.Fixed;

with Keyword_Record; -- String_80 needed

with Ada.Text_IO;-- use Ada.Text_IO;

package body Optional is

package ASF renames Ada.Strings.Fixed;


-- Capacity (later replace with Lists or dynamic vectors)
Arr_Last  : constant Positive := 100;
-- max this many cards searched in one pass

K : Bounded_String_8_Arr(1 .. Arr_Last);-- FIXME starts from 0
C : Card_Arr(1 .. Positive_Count(Arr_Last)); -- FIXME starts from 1 & explicit cast!
-- positions in use in the above arrays
K_Last : Natural := 0; 
C_Last : Count := 0;

-- FIXME everywhere fix array indexes: use 'First 'Last (loops and array asignments :=)
-- FIXME what happens if C-array remains empty (no key match) ? 
-- In Get_Cards: C(1..0) because C_Last remains 0

function Init (Keys : in Bounded_String_8_Arr) return Positive_Count
is
begin
    K_Last := Keys'Last + 1; -- Bounded_String_8_Arr starts from 0
--    Put_Line("HUHU " & Natural'Image(K_Last));
--    for I in Keys'Range
--    loop
--        Put_Line("HUH " & Integer'Image(I) & ": " & BS_8.To_String(Keys(I)));
--    end loop;
    K(1..K_Last) := Keys;
    C_Last := 0;

    return 1;
end Init;


function Next (Pos  : in Positive_Count; Card : in KWR.String_80) return Count
is
--    use SIO;
begin
    if(Card = ENDCard) then return 0; end if;

    for I in 1 .. K_Last
    loop
        declare
            Key : String := BS_8.To_String(K(I));
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




-- Ops on Card_Arr

function Find_Key(Cards : Optional.Card_Arr; Key : BS_8.Bounded_String) return Card_Arr
is
    use Ada.Strings;
    Key8 : String(1..8);
begin
    for I in Cards'Range
    loop
        Key8 := Cards(I)(1..8);
        ASF.Trim(Key8, Both, Left);-- FIXME this must be conversion func Strin->Key_Type
        if(Cards(I)(1..8) = Key8)
        then
            declare
                Card : Card_Arr(1..1) := Cards(I..I);
            begin
                return Card;
            end;
        end if;
    end loop;
    return Null_Card_Arr;
end Find_Key;



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





 procedure VKR_Read (
            Stream : not null access Ada.Streams.Root_Stream_Type'Class;
            Item   : out  Valued_Key_Record)
 is
     Card : String(1..80);
     use Ada.Strings;
 begin
     String'Read(Stream, Card);
     Item.Key   := BS_8.To_Bounded_String(ASF.Trim(Card(1..8),  Both));
     ASF.Trim(Card(11..30), Both, Right);
     Item.Value := BS70.To_Bounded_String(Card(11..30));
 end VKR_Read;



end Optional;

