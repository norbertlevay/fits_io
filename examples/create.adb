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
with Optional;        use Optional;       -- Card_Arr needed
with Header;          use Header;         -- Create_Card needed

with Image;

with Numeric_Type;
with Array_IO;
with File.Misc;-- Write_Padding needed

with Pool_For_Numeric_Type; use Pool_For_Numeric_Type;

procedure create
is

 package TIO renames Ada.Text_IO;
 package SIO renames Ada.Streams.Stream_IO;

 FileName : constant String := Command_Name & ".fits";
 F        : SIO.File_Type;

 -- create Header

 ColLength : constant SIO.Positive_Count := 256;
 RowLength : constant SIO.Positive_Count := 456;
 NAXISn  : NAXIS_Arr := (ColLength, RowLength);

 OptCards  : Card_Arr :=
            (
                Create_Card("DATAMIN",  "0"),
                Create_Card("DATAMAX","255")
            );

 package Im is new Image(Float_32, NAXISn, OptCards);

 -- write data

 type Float_Arr is array (SIO.Positive_Count range <>) of Float;
 type SI_Arr is array (SIO.Positive_Count range <>) of Short_Integer;

 package Phys is new Numeric_Type(Float, Float_Arr, Float_Arr);
 package Raw  is new Numeric_Type(Short_Integer,SI_Arr, Float_Arr);
 package AIO  is new Array_IO(Raw,Phys);

 Column : Float_Arr(1..ColLength);
 --Column : Phys.Numeric_Arr(1..ColLength);
 Data : Phys.Numeric;
 Min  : Phys.Numeric := Phys.Numeric'Last;
 Max  : Phys.Numeric := Phys.Numeric'First;
 use SIO;-- operator "-" on SIO.Count needed
begin

 Put_Line("Writing " & FileName & " ... "); 

 SIO.Create (F, SIO.Out_File, FileName);

 Header.Write_Card_SIMPLE(F, True);
 Header.Write_Cards(F, Im.To_Cards(Raw.BITPIX));
 Header.Close(F);

 -- init phys-array

 for I in Column'Range
 loop
    Column(I) := Phys.To_Numeric(Float(I-1));
    Data := Column(I);
    if(Data < Min) then Min := Data; end if;
    if(Data > Max) then Max := Data; end if;
end loop;

 -- write Data Unit

 for I in 1 .. RowLength
 loop
     AIO.Write(F, 0.0, 1.0, Column);
 end loop;

 File.Misc.Write_Padding(F,SIO.Index(F),File.Misc.DataPadValue);
 SIO.Close(F);

 Put_Line("Min " & Phys.Numeric'Image(Min));
 Put_Line("Max " & Phys.Numeric'Image(Max));

exception
  when Except_ID : others =>
     declare
      Error : Ada.Text_IO.File_Type := Standard_Error;
     begin
      Put_Line(Error, Exception_Information( Except_ID ) );
     end;
end create;

