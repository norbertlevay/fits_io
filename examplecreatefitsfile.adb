
with
    Ada.Exceptions,
    Ada.Text_IO,
    Ada.Text_IO.Bounded_IO,
    Ada.Text_IO.Text_Streams,
    Ada.Streams.Stream_IO,
    Ada.Command_Line,
    Ada.Strings.Unbounded,
    Ada.Strings.Bounded,
    Ada.Strings.Fixed,
    Ada.Unchecked_Conversion,
    System,
    Interfaces,
    GNAT.Traceback.Symbolic;

use
    Ada.Exceptions,
    Ada.Text_IO,
    Ada.Strings.Unbounded,
    Ada.Strings.Bounded,
    Ada.Streams,
    Ada.Streams.Stream_IO,
    System,
    Ada.Command_Line;

with FITS.Header;      use FITS.Header;
with FITS.Size; use FITS.Size;
with FITS.File; use FITS.File;
with FITS.Data; use FITS.Data;

with Interfaces;
use  Interfaces;


procedure exampleCreateFitsFile is

 Name   : String   := Command_Name & ".fits";
 FitsFile : SIO.File_Type;

 HDUNum  : Positive := 1;

 RowsCnt : constant Positive := 600;
 ColsCnt : constant Positive := RowsCnt;
 DataCnt : constant Positive := RowsCnt*ColsCnt;
 DPadCnt : constant Natural  := 2880 - (DataCnt mod 2880);
 -- padding data up to block size

 Data : UInt8Arr_Type(1 .. DataCnt);
 -- if data small enough to handle in memory do as above,
 -- otherwise
 -- writing by blocks:
 -- DBlk : Data_Arr(UInt8,2880);-- := (
 --   FitsType => UInt8,
 --   Length   => 2880,
 --   UInt8Arr => (others   => FITS.Data.Unsigned_8(128)));
 Padding : UInt8Arr_Type(1 .. DPadCnt) := (others => 0);

 -- Header for the above data:
 HBlk : HeaderBlock_Type := (
   1 => To_Card("SIMPLE","1","Standard FITS file"),
   2 => To_Card("BITPIX","8"," "),
   3 => To_Card("NAXIS", "2"," "),
   4 => To_Card("NAXIS1",Positive'Image(RowsCnt)," "),
   5 => To_Card("NAXIS2",Positive'Image(ColsCnt)," "),
   6 => ENDCard,
   others => EmptyCard);

begin

 Put_Line("Usage " & Command_Name );
 New_Line;
 Put_Line(" output: " & Name );
 New_Line(2);

 -- DBG print header
 for I in HBlk'Range
 loop
  Put_Line( HBlk(I) );
 end loop;

 -- generate the data
 for I in 1 .. RowsCnt loop
   for J in 1 .. ColsCnt loop
     Data((I-1)*ColsCnt + J) := FITS.Data.Unsigned_8((I mod 256)/2 + (J mod 256)/2);
   end loop;
 end loop;


 -- create and write the FIST file

 SIO.Create (FitsFile, SIO.Out_File, Name);
 -- FIXME check behaviour AdaRM: overwrites if file already exists ?
 -- FIXME if AdaRM says SIO.Crete guarantees File Index
 -- to be 1 after Create ? Otherwise call Set_Index(FitsFile,1)

 HeaderBlock_Type'Write(Stream(FitsFile),HBlk);

 UInt8Arr_Type'Write(Stream(FitsFile),Data);
 UInt8Arr_Type'Write(Stream(FitsFile),Padding);


 SIO.Close(FitsFile);

 exception

  when Except_ID : others =>
     declare
      Error :  Ada.Text_IO.File_Type := Standard_Error;
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

