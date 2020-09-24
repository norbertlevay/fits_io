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

 A : Float:=0.0;
 B : Float:=1.0;
 package Buff is new Buffer_Type(Float, A,B);

 Column : Buff.Buffer(1..ColLength);


function Generate_Data(I : SIO.Count; ColLength : SIO.Positive_Count) return Buff.Buffer
is
    Column : Buff.Buffer(1..ColLength);
begin
 for I in Column'Range
 loop
    Column(I) := Float(I)-1.0;
 end loop;
 return Column;
end Generate_Data;




begin

 Put_Line("Writing " & FileName & " ... "); 

 SIO.Create (F, SIO.Out_File, FileName);

-- write Header

 Header.Write_Card_SIMPLE(F, True);
 Header.Write_Cards(F, MandCards);
 Valued_Key_Record_Arr'Write(SIO.Stream(F), ArrKeyRecs);
 Header.Close(F);

-- write Data Unit

 for I in 1 .. RowLength
 loop
     Column := Generate_Data(I, ColLength);
     Buff.Buffer'Write(SIO.Stream(F), Column);
 end loop;
 File.Misc.Write_Padding(F,SIO.Index(F),File.Misc.DataPadValue);

 SIO.Close(F);



exception
  when Except_ID : others =>
     declare
      Error : Ada.Text_IO.File_Type := Standard_Error;
     begin
      Put_Line(Error, Exception_Information( Except_ID ) );
     end;
end create;

