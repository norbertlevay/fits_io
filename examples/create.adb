--
-- Example create & write small FITS file
--
-- "small" meaning data (and header) fit into memory (heap).

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Traceback.Symbolic;

with Ada.Streams.Stream_IO;

with FITS;        use FITS;
with FITS.Header; use FITS.Header;
with FITS.File;   use FITS.File;


procedure create
is

 package SIO renames Ada.Streams.Stream_IO;

 FileName : constant String := Command_Name & ".fits";
 File     : SIO.File_Type;

 -- Describe the Data

 RowsCnt : constant FPositive := 600;-- = ColumnLength
 ColsCnt : constant FPositive := 400;-- = Row   Length

 MaxCoords : constant NAXIS_Arr := (RowsCnt,ColsCnt);

 -- Prepare the Header

 -- FIXME uses String varinat of To_Card, not bounded strings Max_8.* etc...
 Cards : Card_Arr :=  (
   To_Card ("SIMPLE",   "T", "Standard FITS file"),
   To_Card ("BITPIX",   "8", "Unsigned 8-bit integer data"),
   To_Card ("NAXIS",    "2", "2-dimensional image"),
   To_Card ("NAXIS1", "600", "rows"),
   To_Card ("NAXIS2", "400", "columns")
   );

 -- Define the Data

 function Squares (Coord : in NAXIS_Arr) return Unsigned_8
 is
 begin
  return Unsigned_8(Coord(1)*Coord(2) mod 256);
 end Squares;

 procedure Write_Data_UInt8 is
       new Write_Data(Unsigned_8,UInt8_Arr,Squares);

begin

 Put_Line("Usage  " & Command_Name );
 Put("Writing " & FileName & " ... ");

 SIO.Create (File, SIO.Out_File, FileName);
 -- FIXME check behaviour AdaRM: overwrites if file already exists ?
 -- FIXME if AdaRM says SIO.Create guarantees File Index
 -- to be 1 after Create ? Otherwise call Set_Index(File,1)

 -- write Header
 Write_Cards(File, Cards);
 Write_ENDCard(File);

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

