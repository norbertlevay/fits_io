--
-- Example list info on each HDU in FITS-file
--
-- FIXME temp: for testing of FITS_IO (DUSize calc)
--


with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Traceback.Symbolic;

with Ada.Streams.Stream_IO;  use Ada.Streams.Stream_IO;

with FITS_IO;        use FITS_IO;
with FITS_IO.Header;
with FITS_IO.File;   use FITS_IO.File;


procedure list
is

 package TIO renames Ada.Text_IO;


 InFile      : SIO.File_Type;
 InFileName  : String := Argument(1);
 -- FIXME might raise excpetion before Usage written


  FitsFile : SIO.File_Type;
  HDUNum   : Positive := 1;

 -- instantiate generic FITS_IO.Header for SIO-File
 function SIO_File_Next(File : Ada.Streams.Stream_IO.File_Type) return Card_Block
  is
   HBlk : Card_Block;
  begin
   Card_Block'Read(Stream(File), HBlk);
   return HBlk;
  end SIO_File_Next;
  pragma Inline(SIO_File_Next);

  package SIO_File_Header is new Header(Source_Type =>  Ada.Streams.Stream_IO.File_Type,
                                        Index_Type  =>  Ada.Streams.Stream_IO.Positive_Count,
                                        Next        =>  SIO_File_Next,
                                        Index       =>  Ada.Streams.Stream_IO.Index,
                                        Set_Index   =>  Ada.Streams.Stream_IO.Set_Index);
  use SIO_File_Header;

begin

 Put_Line("Usage  " & Command_Name & " <file name>");

   SIO.Open(InFile, SIO.In_File, InFileName);
   -- Print_Headline;

   while not SIO.End_Of_File(InFile)
   loop
     declare
       --HDUInfo : HDU_Info := FITS_IO.File.List.Get(InFile);
     begin
       TIO.Put_Line(Positive'Image(HDUNum));
       HDUNum := HDUNum + 1;
       Set_Index(InFile, HDUNum);
     end;
   end loop;
   SIO.Close(InFile);

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

   
   


