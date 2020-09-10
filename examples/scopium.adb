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
with Header;          use Header;         -- needed to setup Header-record
with Raw.Data_Unit;                       -- writes data unit
with Physical.Data_Unit;                  -- writes data unit
with V3_Pool_Scaling; use V3_Pool_Scaling;

with File.Misc;       use File.Misc; -- needs Write_Padding for Header

with Image;

procedure scopium
is

 package TIO renames Ada.Text_IO;
 package SIO renames Ada.Streams.Stream_IO;

 FileName : constant String := Command_Name & ".fits";
 File     : SIO.File_Type;

 InFileName : constant String := Argument(1);
 InFile     : SIO.File_Type;



 -- create Header

 ColsCnt : constant SIO.Positive_Count := 640;
 RowsCnt : constant SIO.Positive_Count := 512;
 NAXISn : NAXIS_Arr := (ColsCnt, RowsCnt);

 -- FIXME these vals below should be calculated from InFile or inside Image
 OptCards  : Card_Arr :=
            (
                Create_Card("DATAMIN",  "0"),
                Create_Card("DATAMAX","255")
            );

 package ScImage is new Image(Unsigned_8, NAXISn, OptCards);


 -- callback to generate data values

 BlkCnt : SIO.Positive_Count := 1;
-- is in V3_Types type U8_Arr is array (SIO.Positive_Count range <>) of Unsigned_8;

 procedure DUData(Data : out U8_Arr)
 is
   use SIO; -- NOTE 'mod' "+" : 'operator not visible'
 begin
     U8_Arr'Read(SIO.Stream(InFile),Data);
     BlkCnt := BlkCnt + 1;
 end DUData;

 package U8_Raw is new Raw(Unsigned_8, U8_Arr);
 package U8_Raw_DU is new U8_Raw.Data_Unit;
 procedure U8_Write_Data_Unit is new U8_Raw_DU.Write_Data_Unit(0, DUData);


 -- do in Physical domain (not Raw)

 ElemCnt : SIO.Positive_Count := 1;
 procedure DUElem(Data : out Unsigned_8)
 is
   use SIO; -- NOTE 'mod' "+" : 'operator not visible'
 begin
     Unsigned_8'Read(SIO.Stream(InFile),Data);
     ElemCnt := ElemCnt + 1;
 end DUElem;


 package U8_Phys is new Physical(Unsigned_8, U8_Arr, Float_32, Unsigned_8);
 package U8_Phys_DU is new U8_Phys.Data_Unit;
 procedure U8_Write_Data_Unit_Phys is new U8_Phys_DU.Write_Data_Unit(0, DUElem);
 U8Dummy1_Valid : Boolean := False;
 U8Dummy1 : Unsigned_8;
 U8Dummy2_Valid : Boolean := False;
 U8Dummy2 : Unsigned_8;

begin

 Put_Line("Usage  " & Command_Name );
 Put_Line("Writing " & FileName & " ... ");

 SIO.Open (InFile, SIO.In_File, InFileName);
 SIO.Create (File, SIO.Out_File, FileName);
 -- FIXME check behaviour AdaRM: overwrites if file already exists ?
 -- FIXME if AdaRM says SIO.Create guarantees File Index
 -- to be 1 after Create ? Otherwise call Set_Index(File,1)

 Header.Write_Card_SIMPLE(File, True);
 Header.Write_Cards(File, ScImage.To_Cards);
 Header.Close(File);
 
 U8_Phys.Init_Undef_For_Write(U8Dummy1_Valid, U8Dummy1, U8Dummy2_Valid, U8Dummy2);
 U8_Write_Data_Unit_Phys(File, NAXISn,U8Dummy1,U8Dummy1_Valid,0.0,1.0,U8Dummy2,U8Dummy2_Valid);

 SIO.Close(File);
 SIO.Close(InFile);

 exception

  when Except_ID : others =>
     declare
      Error : Ada.Text_IO.File_Type := Standard_Error;
     begin
      Put_Line(Error, Exception_Information( Except_ID ) );
     end;
end scopium;

