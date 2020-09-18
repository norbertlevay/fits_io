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

--with File.Misc;       use File.Misc; -- needs Write_Padding for Header

with Image;

with Numeric_Type;
with Scaling;
with Scaling.Streams;
with Pool_For_Numeric_Type; use Pool_For_Numeric_Type;

procedure create
is

 package TIO renames Ada.Text_IO;
 package SIO renames Ada.Streams.Stream_IO;

 FileName : constant String := Command_Name & ".fits";
 File     : SIO.File_Type;


 -- create Header

 RowsCnt : constant SIO.Positive_Count := 256;
 ColsCnt : constant SIO.Positive_Count := 256;
 NAXISn  : NAXIS_Arr := (ColsCnt, RowsCnt);

 OptCards  : Card_Arr :=
            (
                Create_Card("DATAMIN",  "0"),
                Create_Card("DATAMAX","255")
            );

 package Im is new Image(Float_32, NAXISn, OptCards);

 -- callback to generate data values

 ColCnt : SIO.Positive_Count := 1;

 Min : Float := Float'Last;
 Max : Float := Float'First;


 package Src is new Numeric_Type(Float);-- FIXME replace with Float_32
 package Dst is new Numeric_Type(Float);-- FIXME replace with Float_32

 procedure DUFLoatData(Data : out Dst.Numeric)
 is
   use SIO; -- NOTE 'mod' "+" : 'operator not visible'
 begin
    Data := Float(ColCnt mod ColsCnt);
    ColCnt := ColCnt + 1;
    if(Data < Min) then Min := Data; end if;
    if(Data > Max) then Max := Data; end if;
 end DUFloatData;

 package SD_Scaling is new Scaling(Src,Dst);
 package F32F32     is new SD_Scaling.Streams;
 procedure F32_Write_Data_Unit is new F32F32.Write_Data_Unit(DUFloatData);

begin

 Put_Line("Usage  " & Command_Name );
 Put_Line("Writing " & FileName & " ... ");

 SIO.Create (File, SIO.Out_File, FileName);

 Header.Write_Card_SIMPLE(File, True);
 Header.Write_Cards(File, Im.To_Cards(-(Float_32'Size)));
 Header.Close(File);

 F32_Write_Data_Unit(File, NAXISn); -- FIXME should be Im.NAXISn; how to do it ?

 SIO.Close(File);

 Put_Line("Min " & Float'Image(Min));
 Put_Line("Max " & Float'Image(Max));

exception
  when Except_ID : others =>
     declare
      Error : Ada.Text_IO.File_Type := Standard_Error;
     begin
      Put_Line(Error, Exception_Information( Except_ID ) );
     end;
end create;

