--
-- Example list info on each HDU in FITS-file
--
-- FIXME temp: for testing of FITS_IO (DUSize calc)
--


with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Traceback.Symbolic;

with Ada.Streams.Stream_IO;

with FITS_IO;        use FITS_IO;
with FITS_IO.Header; use FITS_IO.Header;
with FITS_IO.File;   use FITS_IO.File;


procedure list
is

 package SIO renames Ada.Streams.Stream_IO;
 package TIO renames Ada.Text_IO;

 InFile      : SIO.File_Type;
 InFileName  : String := Argument(1);
 -- FIXME might raise excpetion before Usage written


 procedure Read_Data  is new gen_Read_Data (Data_Arr => UInt8_Arr);
 procedure Write_Data is new gen_Write_Data(Data_Arr => Float32_Arr);

 BITPIXFloat64Card : Card_Type :=
   To_Card (Max_8.To_Bounded_String("BITPIX"),
            Max20.To_Bounded_String("-32"),
            Max48.To_Bounded_String("Float 32 Data Type"));

begin

 Put_Line("Usage  " & Command_Name & " <file name>");

 SIO.Open   (InFile,  SIO.In_File,  InFileName);

 -- interpret header: DataUnit length and type needed
 declare
  HDUInfo : HDU_Info_Type := Get(InFile);
 begin
  BITPIX := HDUInfo.BITPIX;
  DUSize := DU_Size (HDUInfo.NAXISn);
 end;

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
      -- addr2line -e ./fits -a addr1 addr2 ...
     end;
end list;

