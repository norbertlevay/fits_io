--
-- Example create & write small FITS file
-- "small" meaning data (and header) fit into memory (heap).

-- FIXME SIO.Create(): check behaviour AdaRM: overwrites if file already exists ?
-- FIXME if AdaRM says SIO.Create guarantees File Index
-- to be 1 after Create ? Otherwise call Set_Index(File,1)


with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Streams.Stream_IO;

with V3_Types;        use V3_Types;
with Keyword_Record;  use Keyword_Record; -- FPositive needed
with Mandatory;       use Mandatory;      -- NAXISn_Arr needed
with Optional;       -- use Optional;       -- Card_Arr needed
with Optional.Reserved; use Optional.Reserved;
with Header;          use Header;         -- Create_Card needed

with Image;

with Numeric_Type;
with Array_IO;
with File.Misc;-- Write_Padding needed

with Pool_For_Numeric_Type; use Pool_For_Numeric_Type;

with Buffer_Type;

with Ada.Strings.Bounded;


procedure create
is

 package TIO renames Ada.Text_IO;
 package SIO renames Ada.Streams.Stream_IO;
-- package BS renames Ada.Strings.Bounded;

 FileName : constant String := Command_Name & ".fits";
 F        : SIO.File_Type;

 -- create Header

 -- mandatory cards

 ColLength : constant SIO.Positive_Count := 256;
 RowLength : constant SIO.Positive_Count := 456;
 NAXISn  : NAXIS_Arr := (ColLength, RowLength);

 package Im is new Image(Float_32, NAXISn);

 MandCards : Optional.Card_Arr := Im.To_Cards(Short_Integer'Size);
 -- FIXME Tf = Short_Integer, how to specify: with Buffer_Type unknown here! Raw.BITPIX
 -- cfitsio makes here "automatic type conversion"

 -- add some optional/reserved cards

 type Valued_Key_Record_Arr is array (Integer range <>) of Optional.Valued_Key_Record;
 use Optional.BS70;
 ArrKeyRecs : Valued_Key_Record_Arr :=
     (
     (BZERO,    1*    "0.0"),
     (BSCALE,   1*    "1.0"),
     (BLANK,    1* "-32768"),
     (DATAMIN,  1*      "0"),
     (DATAMAX,  1*    "255")
     );


 -- write data

 type Float_Arr is array (SIO.Positive_Count range <>) of Float;

 A : Float:=0.0;
 B : Float:=1.0;
 package Buff is new Buffer_Type(Float, Float_Arr, A,B);

 Column : Float_Arr(1..ColLength);
 --Column : Phys.Numeric_Arr(1..ColLength);
 Data : Float;--Phys.Numeric;
 Min  : Float;--Phys.Numeric := Phys.Numeric'Last;
 Max  : Float;--Phys.Numeric := Phys.Numeric'First;
 use SIO;-- operator "-" on SIO.Count needed
begin

 Put_Line("Writing " & FileName & " ... "); 

 SIO.Create (F, SIO.Out_File, FileName);

 Header.Write_Card_SIMPLE(F, True);
 Header.Write_Cards(F, MandCards);
 Valued_Key_Record_Arr'Write(SIO.Stream(F), ArrKeyRecs);
 Header.Close(F);

 -- init phys-array

 for I in Column'Range
 loop
    Column(I) := Float(I-1);
    --Column(I) := Phys.To_Numeric(Float(I-1));
    Data := Column(I);
    if(Data < Min) then Min := Data; end if;
    if(Data > Max) then Max := Data; end if;
end loop;

 -- write Data Unit

 for I in 1 .. RowLength
 loop
     Buff.Write_Buffer(F, Column);
     --AIO.Write(F, 0.0, 1.0, Column);
 end loop;

 File.Misc.Write_Padding(F,SIO.Index(F),File.Misc.DataPadValue);
 SIO.Close(F);

 Put_Line("Min " & Float'Image(Min));
 Put_Line("Max " & Float'Image(Max));
-- Put_Line("Min " & Phys.Numeric'Image(Min));
-- Put_Line("Max " & Phys.Numeric'Image(Max));


exception
  when Except_ID : others =>
     declare
      Error : Ada.Text_IO.File_Type := Standard_Error;
     begin
      Put_Line(Error, Exception_Information( Except_ID ) );
     end;
end create;

