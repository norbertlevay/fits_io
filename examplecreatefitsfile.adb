
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
 Cnt  : Positive := 5;
-- Data : DataArray_Type(Card,Cnt);
-- Card : Card_Type;
-- BITPIXVal : Integer;
 HDUNum  : Positive := 1;
-- HDUSize : HDU_Size_Type;
-- DUStart : SIO.Count;

 RowsCnt : constant Positive := 600;
 ColsCnt : constant Positive := RowsCnt;
 DataCnt : constant Positive := RowsCnt*ColsCnt;

 HBlk : HeaderBlock_Type := (
   1 => To_Card("SIMPLE","1","Standard FITS file"),
   2 => To_Card("BITPIX","8"," "),
   3 => To_Card("NAXIS", "2"," "),
   4 => To_Card("NAXIS1",Positive'Image(RowsCnt)," "),
   5 => To_Card("NAXIS2",Positive'Image(ColsCnt)," "),
   6 => ENDCard,
   others => EmptyCard);

 Data : UInt8Arr_Type(1 .. DataCnt);

-- DBlk : Data_Arr(UInt8,2880);-- := (
--   FitsType => UInt8,
--   Length   => 2880,
--   UInt8Arr => (others   => FITS.Data.Unsigned_8(128)));
begin

 Put_Line("Usage " & Command_Name );
 New_Line;
 Put_Line(" output: " & Name );
 New_Line(2);

 -- DBG print
 for I in HBlk'Range
 loop
  Put_Line( HBlk(I) );
 end loop;

 -- generate the data as described in Header
 for I in 1 .. RowsCnt loop
   for J in 1 .. ColsCnt loop
     Data((I-1)*ColsCnt + J) := FITS.Data.Unsigned_8((I mod 256)/2 + (J mod 256)/2);
   end loop;
 end loop;

 SIO.Create (FitsFile, SIO.Out_File, Name);
 -- FIXME check behaviour AdaRM: overwrites if file already exists ?

 Set_Index(FitsFile,HDUNum);
 -- FIXME if AdaRM says SIO.Open guarantees File Index
 -- to be 1 after Open this line is not necessary

 HeaderBlock_Type'Write(Stream(FitsFile),HBlk);

-- for I in 1 ..  (1 + DataCnt / 2880)
-- loop
--   for I in DBlk.UInt8Arr'Range
--   loop
--    DBlk.UInt8Arr(I) := FITS.Data.Unsigned_8(I mod 256);
--   end loop;
--   Data_Arr'Write(Stream(FitsFile),DBlk);
-- end loop;

 UInt8Arr_Type'Write(Stream(FitsFile),Data);


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

