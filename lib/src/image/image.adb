
with Ada.Streams.Stream_IO;

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with V3_Types;   use V3_Types; -- Float_32 needed
with Keyword_Record; use Keyword_Record; -- String_80 needed

package body Image is

 package SIO renames Ada.Streams.Stream_IO;


-- NOTE free-format integer must be right justified ? $
-- Standard ambigous Sect 4.2.3; fitsverify: no complain$
function Create_Card(Key : in String; Value : in String) return String_80
is
    C : String(1 .. 80);
    Val_Len : Positive := Value'Length;
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
begin
    Move(Key,   C(1 .. 8));
    Move("= ",  C(9 ..10));
    Move(Value, C(11..30));
    Move(" ",   C(31..80));
    return C;
end Create_Mandatory_Card;



function To_Value_String( V : in Integer) return String
is
    Vstr: String(1 .. 20);
begin
    Move(Integer'Image(V), Vstr, Error, Right);
    return Vstr;
end To_Value_String;



function To_Value_String( V : in SIO.Count) return String
is
    Vstr: String(1 .. 20);
begin
    Move(SIO.Count'Image(V), Vstr, Error, Right);
    return Vstr;
end To_Value_String;



function To_Value_String( V : in Float_32) return String
is
    Vstr: String(1 .. 20);
begin
    Move(Float_32'Image(V), Vstr, Error, Right);
    return Vstr;
end To_Value_String;

function To_Value_String( V : in Boolean) return String
is
    Vstr : String(1 .. 20);
begin
    if(V = True) then
        Move("T", Vstr, Error, Right);
    else
        Move("F", Vstr, Error, Right);
    end if;
    return Vstr;
end To_Value_String;

function Create_NAXIS_Card_Arr(NAXISn : in NAXIS_Arr) return Card_Arr
is
    Cards : Card_Arr(1 .. NAXISn'Last);
begin
    for I in NAXISn'Range
    loop
        Cards(I) := Create_Mandatory_Card("NAXIS" & Trim(Integer'Image(I),Left),
                                        To_Value_String(NAXISn(I)));
    end loop;
    return Cards;
end Create_NAXIS_Card_Arr;


function To_Cards( Im : in Image_Rec ) return Card_Arr
is
    Cards : Card_Arr(1 .. (2 + Im.NAXISn'Length));
begin
    Cards(1) := Create_Mandatory_Card("BITPIX",  To_Value_String(Im.BITPIX));
    Cards(2) := Create_Mandatory_Card("NAXIS",   To_Value_String(Im.NAXIS));
    Cards(3 .. (3 + Im.NAXISn'Length) - 1) := Create_NAXIS_Card_Arr(Im.NAXISn);
    return Cards;
end To_Cards;




-- FIXME consider: Create_First_Card be in separate array and written separately,
-- similar to writing END-card at 'closing' _independently_ of what (IMAGE TABLE etc)
-- is being written into it: Primary is tied 
-- to File_Block_Index=1 and Extension File_Block_Index/=1
-- FIXME later consider make this Write-attrib : Image_Rec'Write

function To_Primary_Cards( Im : in Image_Rec ) return Card_Arr
is
    ImCardsCnt : Positive := 2 + Im.NAXISn'Length;
    Cards : Card_Arr(1 .. (1 + ImCardsCnt));
begin
    Cards(1) := Create_Mandatory_Card("SIMPLE",  To_Value_String(True));
    Cards(2 .. (1 + ImCardsCnt)) := To_Cards(Im);
    return Cards;
end To_Primary_Cards;


function To_Extension_Cards( Im : in Image_Rec ) return Card_Arr
is
    ImCardsCnt : Positive := 2 + Im.NAXISn'Length;
    Cards  : Card_Arr(1 .. (1 + ImCardsCnt + 2 + 1));
    ImLast : Positive := ImCardsCnt;
begin
    Cards(1) := Create_Mandatory_Card("XTENSION",  "'IMAGE   '");
    Cards(2 .. ImLast) := To_Cards(Im);
    Cards(ImLast+1) := Create_Mandatory_Card("PCOUNT", To_Value_String(SIO.Count(0)));
    Cards(ImLast+2) := Create_Mandatory_Card("GCOUNT", To_Value_String(SIO.Count(1)));
    return Cards;
end To_Extension_Cards;


end Image;
