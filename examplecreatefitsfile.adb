
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



with FITS.Header; use FITS.Header;
with FITS.Data;   use FITS.Data;


procedure exampleCreateFitsFile is

 package SIO renames Ada.Streams.Stream_IO;

 Name     : String := Command_Name & ".fits";
 FitsFile : SIO.File_Type;

 RowsCnt : constant Positive := 600;
 ColsCnt : constant Positive := 400;--RowsCnt;
 DataCnt : constant Positive := RowsCnt*ColsCnt;
 DPadCnt : constant Natural  := 2880 - (DataCnt mod 2880);
 -- padding data up to block size

 type Buffer_Type is new UInt8Arr_Type(1 .. DataCnt);
 for Buffer_Type'Size use DataCnt*(FITS.Data.Unsigned_8'Size);

 type Buffer_Ptr  is access Buffer_Type;
 Buffer : Buffer_Ptr;


 -- if data small enough to handle in memory (heap),
 -- otherwise
 -- writing by blocks:
 -- DBlk : Data_Arr(UInt8,2880);-- := (
 --   FitsType => UInt8,
 --   Length   => 2880,
 --   UInt8Arr => (others   => FITS.Data.Unsigned_8(128)));
 Padding : UInt8Arr_Type(1 .. DPadCnt) := (others => 0);

 -- Header for the above data:
 HBlk : HeaderBlock_Type := (
   1 => To_Card("SIMPLE","T","Standard FITS file"),
   2 => To_Card("BITPIX","8"," "),
   3 => To_Card("NAXIS", "2"," "),
   4 => To_Card("NAXIS1",Positive'Image(RowsCnt)," "),
   5 => To_Card("NAXIS2",Positive'Image(ColsCnt)," "),
   6 => ENDCard,
   others => EmptyCard);
   -- data is stored as array where NAXIS1 varies
   -- most rapidly, then NAXIS2, etc...

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

 Buffer := new Buffer_Type;
 -- will be relelased at exit from begin .. end section

 -- generate the data
 -- [FITS 3.4.1. Fig 1] NAXIS1 (Rows) varies most rapidly
 for J in 1 .. ColsCnt loop
   for I in 1 .. RowsCnt loop
     Buffer.all((J-1)*RowsCnt + I) := FITS.Data.Unsigned_8((I mod 256)/2 + (J mod 256)/2);
   end loop;
 end loop;


 -- create and write the FIST file

 SIO.Create (FitsFile, SIO.Out_File, Name);
 -- FIXME check behaviour AdaRM: overwrites if file already exists ?
 -- FIXME if AdaRM says SIO.Crete guarantees File Index
 -- to be 1 after Create ? Otherwise call Set_Index(FitsFile,1)

 HeaderBlock_Type'Write(SIO.Stream(FitsFile),HBlk);

 Buffer_Type'Write(SIO.Stream(FitsFile),Buffer.all);
 if DPadCnt /= 2880 then
  UInt8Arr_Type'Write(SIO.Stream(FitsFile),Padding);
 end if;

 SIO.Close(FitsFile);

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

