--
-- Example create & write small FITS file
--
-- "small" meaning data (and header) fit into memory (heap).

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Traceback.Symbolic;

with Ada.Streams.Stream_IO;

with Data_Types;   use Data_Types;
with File.Misc;   use File.Misc;
with Keyword_Record; use Keyword_Record; -- FPositive needed
with Strict; use Strict; -- Positive_Arr needed
with Optional; use Optional; -- Card_Arr & ENDCard needed 

procedure create
is

 package SIO renames Ada.Streams.Stream_IO;

 FileName : constant String := Command_Name & ".fits";
 File     : SIO.File_Type;

 -- Describe the Data

 RowsCnt : constant Positive := 500;-- = ColumnLength
 ColsCnt : constant Positive := 500;-- = Row   Length

-- MaxCoords : constant Positive_Arr := (FPositive(RowsCnt),ColsCnt);

 -- Prepare the Header

 -- Card => Key Value Comment
 Cards : Card_Arr :=  (
"SIMPLE  =                    T / Standard FITS FIle                             ",
"BITPIX  =                  -32 /                                                ",
"NAXIS   =                    2 /                                                ",
"NAXIS1  =                  500 /                                                ",
"NAXIS2  =                  500 /                                                ",
"DATAMIN =                    0 /                                                ",
"DATAMAX =                  255 /                                                ",
   ENDCard
   );


 function SomeData(OffInDU : Positive) return Float_32
 is
 begin
   return Float_32(OffInDU mod 256);
 end SomeData;

 procedure F32_Write_Data_Unit is
       new Write_Data_Unit(Float_32,SomeData);

 NDataElems : constant Positive := RowsCnt*ColsCnt;

begin

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

