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

 RowsCnt : constant FPositive := 500;-- = ColumnLength
 ColsCnt : constant FPositive := 500;-- = Row   Length

 MaxCoords : constant Positive_Arr := (RowsCnt,ColsCnt);

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

 -- Define the Data

 function Squares (Coord : in Positive_Arr) return Float_32
 is
	PRes : FNatural :=  Coord(Coord'First)*Coord(Coord'Last) mod 256;
	FRes : Float_32 := Float_32(PRes);
 begin

 --  Put_Line(FNatural'Image(PRes) & " vs " & Float_32'Image(FRes));

  return FRes;
 end Squares;

 procedure Write_Data_F32 is
       new Write_DataUnit(Float_32,Squares);

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
 Write_Data_F32(File, MaxCoords);

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

