--
-- Example create & write small FITS file
--
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
with Optional;        use Optional;       -- Card_Arr & ENDCard needed 
with Header;          use Header;         -- needed to setup Header-record
with Raw.Data_Unit;                       -- writes data unit

with File.Misc;       use File.Misc; -- needs Write_Padding for Header

with Image;

procedure create
is

 package TIO renames Ada.Text_IO;
 package SIO renames Ada.Streams.Stream_IO;

 FileName : constant String := Command_Name & ".fits";
 File     : SIO.File_Type;


 -- create Header

 RowsCnt : constant SIO.Positive_Count := 500;
 ColsCnt : constant SIO.Positive_Count := 500;
 NAXISn  : NAXIS_Arr := (ColsCnt, RowsCnt);

 OptCards  : Card_Arr :=
            (
                Create_Card("DATAMIN",  "0"),
                Create_Card("DATAMAX","255")
            );

 package Im is new Image(Float_32, NAXISn, OptCards);


 -- callback to generate data values

 ColCnt : SIO.Positive_Count := 1;
 type F32_Arr is array (SIO.Positive_Count range <>) of Float_32;

 procedure DUFLoatData(Data : out F32_Arr)
 is
   use SIO; -- NOTE 'mod' "+" : 'operator not visible'
 begin
  for I in Data'Range
  loop
    Data(I) := Float_32(ColCnt mod ColsCnt);
    ColCnt := ColCnt + 1;
  end loop;
 end DUFloatData;

 package F32_Raw is new Raw(Float_32, F32_Arr);
 package F32_Raw_DU is new F32_Raw.Data_Unit;
 procedure F32_Write_Data_Unit is new F32_Raw_DU.Write_Data_Unit(0.0, DUFloatData);

begin

 Put_Line("Usage  " & Command_Name );
 Put_Line("Writing " & FileName & " ... ");

 SIO.Create (File, SIO.Out_File, FileName);

 Header.Write_Card_SIMPLE(File, True);
 Header.Write_Cards(File, Im.To_Cards);
 Header.Close(File);

 F32_Write_Data_Unit(File, NAXISn); -- FIXME should be Im.NAXISn; how to do it ?

 SIO.Close(File);

 exception
  when Except_ID : others =>
     declare
      Error : Ada.Text_IO.File_Type := Standard_Error;
     begin
      Put_Line(Error, Exception_Information( Except_ID ) );
     end;
end create;

