--
-- Example create & write small FITS file
--
-- "small" meaning data (and header) fit into memory (heap).

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Traceback.Symbolic;

with Ada.Streams.Stream_IO;

with V3_Types;   use V3_Types;
with File.Misc;   use File.Misc;
with Keyword_Record; use Keyword_Record; -- FPositive needed
with Mandatory; use Mandatory; -- Positive_Arr needed
with Optional; use Optional; -- Card_Arr & ENDCard needed 

-- NEW BEGIN
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
-- NEW END

procedure create
is

 package TIO renames Ada.Text_IO;
 package SIO renames Ada.Streams.Stream_IO;
 use SIO; -- NOTE because 'operator not visible'

 FileName : constant String := Command_Name & ".fits";
 File     : SIO.File_Type;


------------------------------------------------------------------------------------------
-- BEGIN this should go to lib

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


-- END this should go to lib
------------------------------------------------------------------------------------------



 -- Describe the Data

 RowsCnt : constant SIO.Positive_Count := 500;-- = ColumnLength
 ColsCnt : constant SIO.Positive_Count := 500;-- = Row   Length

-- FIXME lessons learned: 
-- below: TFIELDS_Last and CardsCount should not be required to be given
Mand : Mandatory.Result_Rec
    := (HDU =>IMAGE, NAXIS_Last => 2, TFIELDS_Last => 0,
        CardsCount => 8, BITPIX => -32, NAXISn => (RowsCnt, ColsCnt));

-- FIXME there should be function to convert Result_Rec -> Card_Arr

Cards : Card_Arr(1 .. Positive(Mand.CardsCount)) -- FIXME conversion
    := (
    Create_Mandatory_Card("SIMPLE",  To_Value_String(True)),
    Create_Mandatory_Card("BITPIX",  To_Value_String(Mand.BITPIX)),
    Create_Mandatory_Card("NAXIS",   To_Value_String(Natural(Mand.NAXIS_Last))),-- FIXME
    Create_Mandatory_Card("NAXIS1",  To_Value_String(Positive(Mand.NAXISn(1)))),-- FIXME
    Create_Mandatory_Card("NAXIS2",  To_Value_String(Positive(Mand.NAXISn(2)))),-- FIXME convs
    Create_Mandatory_Card("DATAMIN", To_Value_String(0)),
    Create_Mandatory_Card("DATAMAX", To_Value_String(255)),
    ENDCard
    );




 function SomeData(OffInDU : SIO.Positive_Count) return Float_32
 is
 begin
   return Float_32(OffInDU mod 256);
 end SomeData;

 procedure F32_Write_Data_Unit is
       new Write_Data_Unit(Float_32,0.0,SomeData);
 -- NOTE IEEE float represents +0.0 as signbit=0 Exp=0 Fraction=0 e.g. fully zero bit array
 -- which is the same as defineition of Pad Value for Daua Unit in FITS standard
 NDataElems : constant SIO.Positive_Count := RowsCnt*ColsCnt;

begin

-- NEW BEGIN
for I in Cards'Range
loop
    Put_Line("DBG NEW >"& Cards(I) & "<");
end loop;


-- NEW END

 Put_Line("Usage  " & Command_Name );
 Put("Writing " & FileName & " ... ");

 SIO.Create (File, SIO.Out_File, FileName);
 -- FIXME check behaviour AdaRM: overwrites if file already exists ?
 -- FIXME if AdaRM says SIO.Create guarantees File Index
 -- to be 1 after Create ? Otherwise call Set_Index(File,1)

 -- write Header
 Card_Arr'Write(SIO.Stream(File),Cards);
 Write_Padding(File,SIO.Index(File),HeaderPadValue);

 -- write Data sequentially

 F32_Write_Data_Unit(File, NDataElems);

 SIO.Close(File);

 Put_Line("done.");


 exception

  when Except_ID : others =>
     declare
      Error : Ada.Text_IO.File_Type := Standard_Error;
     begin
      New_Line(Error);
      Put_Line(Error, "Exception_Information: ");
      Put_Line(Error, Exception_Information( Except_ID ) );
      New_Line(Error);
      Put_Line(Error, "Call stack traceback symbols: addr2line -e ./fits addr1 addr2 ...");
      Put_Line(" > Trace-back of call stack: " );
      Put_Line( GNAT.Traceback.Symbolic.Symbolic_Traceback(Except_ID) );
      -- See more at: http://compgroups.net/comp.lang.ada/gnat-symbolic-traceback-on-exceptions/1409155#sthash.lNdkTjq6.dpuf
      -- Do the same manually, use:
      -- addr2line -e ./fits addr1 addr2 ...
     end;
end create;

