--
-- Example create & write small FITS file
--
-- "small" meaning data (and header) fit into memory (heap).

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Streams.Stream_IO;

with V3_Types;        use V3_Types;
with Keyword_Record;  use Keyword_Record; -- FPositive needed
with Mandatory;       use Mandatory;      -- NAXISn_Arr needed
with Optional;        use Optional;       -- Card_Arr & ENDCard needed 
with Image;           use Image;          -- needed to setup Header-record
with Raw;                                 -- writes data unit

with File.Misc;       use File.Misc; -- needs Write_Padding for Header

procedure create
is

 package TIO renames Ada.Text_IO;
 package SIO renames Ada.Streams.Stream_IO;
 use SIO; -- NOTE because 'operator not visible'

 FileName : constant String := Command_Name & ".fits";
 File     : SIO.File_Type;


 -- create Header

 RowsCnt : constant SIO.Positive_Count := 500;
 ColsCnt : constant SIO.Positive_Count := 500;

 Im    : Image_Rec := (NAXIS => 2, BITPIX => -32, NAXISn => (RowsCnt, ColsCnt));
 MandCards : Card_Arr  := To_Primary_Cards(Im);
 OptCards  : Card_Arr :=
            (
                Create_Card("DATAMIN",  "0"),
                Create_Card("DATAMAX","255")
            );

 Cards : Card_Arr := (MandCards & OptCards & ENDCard);


 -- generate data values

 ColCnt : SIO.Positive_Count := 1;
 type F32_Arr is array (SIO.Positive_Count range <>) of Float_32;

 procedure DUFLoatData(Data : out F32_Arr)
 is
 begin
  for I in Data'Range
  loop
    Data(I) := Float_32(ColCnt mod ColsCnt);
    ColCnt := ColCnt + 1;
  end loop;
 end DUFloatData;

 package F32_Raw is new Raw(Float_32, F32_Arr);
 procedure F32_Write_Data_Unit is new F32_Raw.Write_Data_Unit(0.0, DUFloatData);

 NDataElems : constant SIO.Positive_Count := RowsCnt*ColsCnt;


begin

 Put_Line("Usage  " & Command_Name );
 Put_Line("Writing " & FileName & " ... ");

 SIO.Create (File, SIO.Out_File, FileName);
 -- FIXME check behaviour AdaRM: overwrites if file already exists ?
 -- FIXME if AdaRM says SIO.Create guarantees File Index
 -- to be 1 after Create ? Otherwise call Set_Index(File,1)

 -- write Header
 Card_Arr'Write(SIO.Stream(File),Cards);
 Write_Padding(File,SIO.Index(File),HeaderPadValue);

 -- write Data Unit sequentially
 F32_Write_Data_Unit(File, Im.NAXISn);

 SIO.Close(File);

 exception

  when Except_ID : others =>
     declare
      Error : Ada.Text_IO.File_Type := Standard_Error;
     begin
      New_Line(Error);
      Put_Line(Error, "Exception_Information: ");
      Put_Line(Error, Exception_Information( Except_ID ) );
      New_Line(Error);
     end;
end create;

