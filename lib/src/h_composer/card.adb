


with Ada.Text_IO;


with FITS_IO; use FITS_IO;
with Ada.Streams.Stream_IO;-- use Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;     --use Ada.Strings.Fixed;
with Ada.Strings.Bounded;   --use Ada.Strings.Bounded;
with Ada.Strings; use Ada.Strings;
with Ada.Unchecked_Deallocation;

with Mandatory;
with File; use File;
with File.Misc; -- needs Write_Padding for Header
with File_Funcs;


package body Card is

    package TIO renames Ada.Text_IO;


--    CardsCntInBlock : constant Positive := 36;
    -- FIXME Card_Block is used in File.Misc::Copy_Blocks & Copy_HDU
    -- find other solution for File.Misc, here move it inside body
--    type Card_Block is array (Positive range 1..CardsCntInBlock) of String_80;
--    pragma Pack (Card_Block);
    -- FIXME does Pack guarantee arr is packed? how to guarantee Arrs are packed
    -- OR do we need to guarantee at all ?






function Has_Card(Cards : Card_Arr; Key : String; Value : out String) return Boolean
is
    Found : Boolean := False;
begin
    for I in Cards'Range
    loop
        if(Cards(I)(1..8) = Key(Key'First .. (Key'First+7)))
        then
            Value := Cards(I)(11..30);
            Found := True;
        end if;
    end loop;
    return Found;
end Has_Card;





-- NOTE free-format integer must be right justified ?
-- Standard ambigous Sect 4.2.3; fitsverify: no complain
function Create_Card(Key : in String; Value : in String) return String_80
is
    C : String(1 .. 80);
    Val_Len : Positive := Value'Length;
    use Ada.Strings.Fixed;
begin
    Move(Key,   C(1  .. 8));
    Move("= ",  C(9  ..10));
 --   Move(Value, C(11 ..(10 + Value'Length)));
    Move(" ",   C(11 .. 80));
    Move(Value, C( (1 + 30 - Value'Length) .. 30 ) );
--    Move(" ",   C((11 + Value'Length) .. 80));
    return C;
end Create_Card;



function Create_Mandatory_Card(Key : in String; Value : in String) return String_80
is
    C : String(1 .. 80);
    use Ada.Strings.Fixed;
begin
    Move(Key,   C(1 .. 8));
    Move("= ",  C(9 ..10));
    Move(Value, C(11..30));
    Move(" ",   C(31..80));
    return C;
end Create_Mandatory_Card;


function To_Value_String( V : in String) return String
is
    Vstr: String(1 .. 20);
    use Ada.Strings.Fixed;
begin
    Move(V, Vstr, Error, Right);
    return Vstr;
end To_Value_String;


function To_Value_String( V : in Integer) return String
is
    Vstr: String(1 .. 20);
    use Ada.Strings.Fixed;
begin
    Move(Integer'Image(V), Vstr, Error, Right);
    return Vstr;
end To_Value_String;



function To_Value_String( V : in Count) return String
is
    Vstr: String(1 .. 20);
begin
    Ada.Strings.Fixed.Move(Count'Image(V), Vstr, Error, Right);
    return Vstr;
end To_Value_String;

--function To_Value_String( V : in Float_32) return String
--is
--    Vstr: String(1 .. 20);
--begin
--    Move(Float_32'Image(V), Vstr, Error, Right);
--    return Vstr;
--end To_Value_String;

function To_Value_String( V : in Boolean) return String
is
    Vstr : String(1 .. 20);
    use Ada.Strings.Fixed;
begin
    if(V = True) then
        Move("T", Vstr, Error, Right);
    else
        Move("F", Vstr, Error, Right);
    end if;
    return Vstr;
end To_Value_String;

function Create_NAXIS_Card_Arr(NAXISn : in NAXIS_Array) return String_80_Array
is
   -- FIXME explicit conversion
    Cards : String_80_Array(1 .. Positive_Count(NAXISn'Last));
    use Ada.Strings.Fixed;
begin
    for I in NAXISn'Range
    loop
        Cards(Positive_Count(I)) := Create_Mandatory_Card("NAXIS" & Trim(Integer'Image(I),Left),
                                        To_Value_String(NAXISn(I)));
    end loop;
    return Cards;
end Create_NAXIS_Card_Arr;


end Card;

