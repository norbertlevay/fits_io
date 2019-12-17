--
-- Example create & write small FITS file
--
-- "small" meaning data (and header) fit into memory (heap).

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Traceback.Symbolic;

with Ada.Streams.Stream_IO;

with FITS;   use FITS;
with File;   use File;
with Keyword_Record; use Keyword_Record; -- FPositive needed
with Strict; use Strict; -- Positive_Arr needed
with Optional; use Optional; -- Card_Arr & ENDCard needed 

procedure create
is

 package SIO renames Ada.Streams.Stream_IO;

 FileName : constant String := Command_Name & ".fits";
 File     : SIO.File_Type;

 -- Describe the Data

 RowsCnt : constant FPositive := 500;-- = ColumnLength
 ColsCnt : constant FPositive := 500;-- = Row   Length

 MaxCoords : constant Positive_Arr := (RowsCnt,ColsCnt);

 -- Prepare the Header

 -- Card => Key Value Comment
 Cards : Card_Arr :=  (
"SIMPLE  =                    T / Standard FITS FIle                             ",
"BITPIX  =                    8 / Standard FITS FIle                             ",
"NAXIS   =                    2 / Standard FITS FIle                             ",
"NAXIS1  =                  500 / Standard FITS FIle                             ",
"NAXIS2  =                  500 / Standard FITS FIle                             ",
   ENDCard
   );

 -- Define the Data

 function Squares (Coord : in Positive_Arr) return Unsigned_8
 is
 begin
  return Unsigned_8(Coord(Coord'First)*Coord(Coord'Last) mod 256);
 end Squares;

 procedure Write_Data_UInt8 is
       new Write_DataUnit(Unsigned_8,Squares);

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

 -- write Data
 Write_Data_UInt8(File, MaxCoords);

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

