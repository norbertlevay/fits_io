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


with FITS.Size;   use FITS.Size;
with FITS.Header; use FITS.Header;
with FITS.Data;   use FITS.Data;


procedure exampleCreateFitsFile
is

 package SIO renames Ada.Streams.Stream_IO;

 Name : constant String := Command_Name & ".fits";
 File : SIO.File_Type;

 -- Describe the Data

 RowsCnt : constant FPositive := 400;
 ColsCnt : constant FPositive := 600;
 DataCnt : constant FPositive := RowsCnt*ColsCnt;
 DPadCnt : constant Positive  := 2880 - Natural(DataCnt mod FPositive(2880));
 -- padding data up to block size

 type Buffer_Type is new UInt8Arr_Type(1 .. DataCnt);
 for  Buffer_Type'Size use DataCnt*(FITS.Data.Unsigned_8'Size);
 -- pack the data before write

 type Buffer_Ptr  is access Buffer_Type;
 Buffer : Buffer_Ptr;

 Padding : constant UInt8Arr_Type(1 .. FPositive(DPadCnt)) := (others => 0);
 for Padding'Size use DPadCnt*(FITS.Data.Unsigned_8'Size);

 -- Prepare the Header

 Cards : Card_Arr := Write_Cards_For_Size
                      (BITPIX => 8,
                       Dim    => (RowsCnt, ColsCnt) );
 HPadCnt : constant Positive      := CardsCntInBlock - Positive((1+Cards'Length) mod CardsCntInBlock);
                                                      -- 1+ is for ENDCard
 HPad    : Card_Arr(1 .. HPadCnt) := (others => EmptyCard);
 -- padding Header up to block size

 -- FIXME how to guarantee Cards & HPad arrays are packed ?

begin

 Put_Line("Usage  " & Command_Name );

 Buffer := new Buffer_Type;
 -- will be released at exit from begin...end section

 -- generate example data
 -- [FITS 3.4.1. Fig 1] NAXIS1 (Rows) varies most rapidly
 for J in 1 .. ColsCnt loop
   for I in 1 .. RowsCnt loop
     Buffer.all((J-1)*RowsCnt + I) := FITS.Data.Unsigned_8((I mod 256)/2 + (J mod 256)/2);
   end loop;
 end loop;


 -- create and write the FITS file

 Put("Output " & Name & " ...");

 SIO.Create (File, SIO.Out_File, Name);
 -- FIXME check behaviour AdaRM: overwrites if file already exists ?
 -- FIXME if AdaRM says SIO.Create guarantees File Index
 -- to be 1 after Create ? Otherwise call Set_Index(File,1)


 -- write Header

 Card_Arr'Write(SIO.Stream(File),Cards);
 Card_Type'Write(SIO.Stream(File),ENDCard);
 if HPadCnt /= CardsCntInBlock then
   Card_Arr'Write(SIO.Stream(File),HPad);
 end if;

 -- write Data

 Buffer_Type'Write(SIO.Stream(File),Buffer.all);
 if DPadCnt /= 2880 then
  UInt8Arr_Type'Write(SIO.Stream(File),Padding);
 end if;

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

