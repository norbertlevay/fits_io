--
-- Example create & write small FITS file
--
-- "small" meaning data (and header) fit into memory (heap).

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Traceback.Symbolic;

with Ada.Streams.Stream_IO;

with Ada.Strings;
with Ada.Strings.Fixed;

with FITS;        use FITS;
--with FITS.Header; use FITS.Header;
with FITS.Keyword; use FITS.Keyword; -- Max_8 ... needed
with FITS.File;   use FITS.File;


procedure create
is

   function To_Card (Key     : in Max_8.Bounded_String;
                     Value   : in Max20.Bounded_String;
                     Comment : in Max48.Bounded_String)
                     return Card_Type
   is
    Card : Card_Type := EmptyCard;
   begin
    -- FIXME how to guarantee Key and Comment are right justified
    --       Value (often) left justified

--    Card(1 .. 8) := Max_8.To_String(Key);
    Ada.Strings.Fixed.Move (Source  => Max_8.To_String(Key),
                        Target  => Card(1 .. 8),
                        Justify => Ada.Strings.Left, 
                        Drop    => Ada.Strings.Error, 
                        Pad     => ' '); 


    Card(9 ..10) := "= ";
    -- Card(11..30) := Max20.To_String(Value);
    Ada.Strings.Fixed.Move (Source  => Max20.To_String(Value),
                        Target  => Card(11 .. 30),
                        Justify => Ada.Strings.Right,
                        Drop    => Ada.Strings.Error, 
                        Pad     => ' '); 
    Card(31..32) := " /"; -- [FITS 4.1.2.3: "Space strongly recommended" ]
--    Card(33..80) := Max48.To_String(Comment);
    Ada.Strings.Fixed.Move (Source  => Max48.To_String(Comment),
                        Target  => Card(33 .. 80),
                        Justify => Ada.Strings.Left,
                        Drop    => Ada.Strings.Error, 
                        Pad     => ' '); 
    return Card;
   end To_Card;


 package SIO renames Ada.Streams.Stream_IO;

 FileName : constant String := Command_Name & ".fits";
 File     : SIO.File_Type;

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
            Max48.To_Bounded_String("columns")),
   ENDCard
   );

 -- Define the Data

 function Squares (Coord : in NAXIS_Arr) return Unsigned_8
 is
 begin
  return Unsigned_8(Coord(1)*Coord(2) mod 256);
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
 Write_Cards(File, Cards);
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

