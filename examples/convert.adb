--
-- Example convert data to Float_64 type
--
-- demonstrate usage if data unit is "big":
-- all data will not fit into memory, needs to be processed
-- in chunks.


with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Traceback.Symbolic;

with Ada.Streams.Stream_IO;

with FITS;        use FITS;
with FITS.Header; use FITS.Header;
with FITS.File;   use FITS.File;


procedure convert
is

 package SIO renames Ada.Streams.Stream_IO;

 OutFileName : constant String := Command_Name & ".fits";
 OutFile     : SIO.File_Type;
 InFile      : SIO.File_Type;

 -- Describe the Data

 RowsCnt : constant FPositive := 600;-- = ColumnLength
 ColsCnt : constant FPositive := 400;-- = Row   Length

 MaxCoords : constant NAXIS_Arr := (RowsCnt,ColsCnt);

 -- Prepare the Header

 -- Card => Key Value Comment
 Cards : Card_Arr :=  (
   To_Card (Max_8.To_Bounded_String("SIMPLE"),
            Max20.To_Bounded_String("T"),
            Max48.To_Bounded_String("Standard FITS file")),

   To_Card (Max_8.To_Bounded_String("BITPIX"),
            Max20.To_Bounded_String("8"),
            Max48.To_Bounded_String("Unsigned 8-bit integer data")),

   To_Card (Max_8.To_Bounded_String("NAXIS"),
            Max20.To_Bounded_String("2"),
            Max48.To_Bounded_String("2-dimensional image")),

   To_Card (Max_8.To_Bounded_String("NAXIS1"),
            Max20.To_Bounded_String("600"),
            Max48.To_Bounded_String("rows")),

   To_Card (Max_8.To_Bounded_String("NAXIS2"),
            Max20.To_Bounded_String("400"),
            Max48.To_Bounded_String("columns"))
   );

 -- Define the Data

 function Squares (Coord : in NAXIS_Arr) return Unsigned_8
 is
 begin
  return Unsigned_8(Coord(1)*Coord(2) mod 256);
 end Squares;

 procedure Write_Data_UInt8 is
       new Write_Data(Unsigned_8,UInt8_Arr,Squares);

 InFileName : String := Argument(1);
 -- FIXME might raise excpetion before Usage written
begin

 Put_Line("Usage  " & Command_Name & " <file name>");
 Put("Writing " & OutFileName & " ... ");

 SIO.Open (InFile, SIO.In_File, InFileName);
 SIO.Create (OutFile, SIO.Out_File, OutFileName);
 -- FIXME check behaviour AdaRM: overwrites if file already exists ?
 -- FIXME if AdaRM says SIO.Create guarantees File Index
 -- to be 1 after Create ? Otherwise call Set_Index(File,1)

 -- FIXME TODO here Read data chunk, convert, write

 -- write Header
 Write_Cards(OutFile, Cards);
 Write_ENDCard(OutFile);

 -- write Data
 Write_Data_UInt8(OutFile, MaxCoords);

 SIO.Close(OutFile);
 SIO.Close(InFile);

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
end convert;

