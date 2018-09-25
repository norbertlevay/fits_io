--
-- Example create & write small FITS file
--
-- if data small enough to handle in memory (heap),
-- otherwise
-- writing by blocks:
-- DBlk : Data_Arr(UInt8,2880);-- := (
--   FitsType => UInt8,
--   Length   => 2880,
--   UInt8Arr => (others   => FITS.Data.Unsigned_8(128)));
--

with
    Ada.Exceptions,
    Ada.Text_IO,
    Ada.Command_Line,
    Ada.Streams.Stream_IO,
    System,
    Interfaces,
    GNAT.Traceback.Symbolic;

use
    Ada.Exceptions,
    Ada.Text_IO,
    Ada.Command_Line;


with FITS;        use FITS;
with FITS.Size;   use FITS.Size;
with FITS.Header; use FITS.Header;
with FITS.Data;   use FITS.Data;
with FITS.File;   use FITS.File;


procedure exampleCreateFitsFile
is

 package SIO renames Ada.Streams.Stream_IO;

 Name : constant String := Command_Name & ".fits";
 File : SIO.File_Type;

 -- Describe the Data

 RowsCnt : constant FPositive := 400;
 ColsCnt : constant FPositive := 600;

 MaxCoords : constant Coord_Arr := (RowsCnt,ColsCnt);

 -- Prepare the Header

 Cards : Card_Arr :=  (
   To_Card ("SIMPLE",   "T", "Standard FITS file"),
   To_Card ("BITPIX",   "8", "Unsigned 8-bit integer data"),
   To_Card ("NAXIS",    "2", "2-dimensional image"),
   To_Card ("NAXIS1", "400", "columns"),
   To_Card ("NAXIS2", "600", "rows")
   );

 -- Define the Data

 function Squares (Coord : in Coord_Arr) return Unsigned_8
 is
 begin
  return Unsigned_8(Coord(1)*Coord(2) mod 256);
 end Squares;

 procedure Write_Data_UInt8 is
       new Write_Data(Unsigned_8,UInt8_Arr,Squares);

begin

 Put_Line("Usage  " & Command_Name );

 -- create and write the FITS file

 Put("Output " & Name & " ...");

 SIO.Create (File, SIO.Out_File, Name);
 -- FIXME check behaviour AdaRM: overwrites if file already exists ?
 -- FIXME if AdaRM says SIO.Create guarantees File Index
 -- to be 1 after Create ? Otherwise call Set_Index(File,1)


 -- write Header
 Card_Arr'Write(SIO.Stream(File),Cards);
 Write_ENDCard(File);


 -- write Data
 Write_Data_UInt8(File,MaxCoords);

 SIO.Close(File);

 Put_Line(" writen.");


 exception

  when Except_ID : others =>
     declare
      Error : Ada.Text_IO.File_Type := Standard_Error;
     begin
      New_Line(Error);
      Put_Line(Error, "Program error, send bug-report.");
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
end exampleCreateFitsFile;

