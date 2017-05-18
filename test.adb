with
    Build_Date,
    Commands,
    Ada.Exceptions,
    Ada.Text_IO,
    Ada.Direct_IO,
    Ada.Text_IO.Bounded_IO,
    Ada.Command_Line,
    Ada.Strings.Unbounded,
    Ada.Strings.Bounded,
    GNAT.Traceback.Symbolic;


use
    Commands,
    Ada.Exceptions,
    Ada.Text_IO,
    Ada.Strings.Unbounded,
    Ada.Strings.Bounded,
    Ada.Command_Line;

with Fits_IO; use Fits_IO;
--with HDU;


procedure test
is
 --
 -- count lines in Header file
 --
 function NoOfLines( FileName : in String )
  return Positive
 is
  FileHandle     : Ada.Text_IO.File_Type;
  NoLines : Natural := 0;
  ENDFound : Boolean := False;
 begin
    Ada.Text_IO.Open( FileHandle, Ada.Text_IO.In_File, FileName );
    loop
      exit when ENDFound;
        declare
         Dummy : String := Ada.Text_IO.Get_Line( FileHandle );
         -- raises exception if no END-Line found but EOF reached
        begin
         if Dummy'Length >= 3 then
           ENDFound := (Dummy(1..3) = "END");
         end if;
        end;
        NoLines := NoLines + 1;
    end loop;
    Ada.Text_IO.Close(FileHandle);
  return NoLines;
 end NoOfLines;
 --
 -- Convert Header from text file into Header_Type
 --
 function Read_HeaderFromTextFile( FileName : in String )
  return Header_Type
 is
   FileHandle : Ada.Text_IO.File_Type;
   NoLines : Positive := NoOfLines(FileName); -- constraint exception if NoLines = 0
   Header  : Header_Type(1 .. NoLines);
 begin
   Ada.Text_IO.Open( FileHandle, Ada.Text_IO.In_File, FileName );
   for I in Header'Range loop
      Header(I) := SB.To_Bounded_String(Ada.Text_IO.Get_Line( FileHandle ));
   end loop;
   Ada.Text_IO.Close(FileHandle);
   return Header;
 end Read_HeaderFromTextFile;


 Fits : Fits_IO.File_Type;
 Mode : Fits_IO.File_Mode := Append_File;
 Name : String    := "test.fits";
 Header : Header_Type :=
  (SB.To_Bounded_String("BITPIX  = 32"),
   SB.To_Bounded_String("NAXIS   = 2"),
   SB.To_Bounded_String("NAXIS1  = 10"),
   SB.To_Bounded_String("NAXIS2  = 10"),
   SB.To_Bounded_String("END"));
 RealHeader : Header_Type := Read_HeaderFromTextFile("realheader.hdr");
begin

 Create (Fits, Mode, Name);
 Ada.Text_IO.Put_Line("Write Primary Header...");
 Write (Fits, RealHeader);
 Ada.Text_IO.Put_Line("Write Header to 2nd HDU...");
 Write (Fits, Header, 2);

 Close(Fits);

 --
 -- error handling
 --
 exception
  when Except_ID : others =>
     declare
      Error :  Ada.Text_IO.File_Type := Standard_Error;
     begin
      Put_Line(Error, "Program error, send a bug-report:");
      New_Line(Error);
      Put_Line(Error, "Exception_Name: " & Exception_Name( Except_ID ) );
      Put_Line(Error, "Exception_Message: " & Exception_Message( Except_ID ) );
      Put_Line(Error, "Exception_Information: ");
      Put_Line(Error, Exception_Information( Except_ID ) );
      New_Line(Error);
      Put_Line(Error, "Call stack traceback symbols: addr2line -e ./fits addr1 addr2 ...");
      --Put_Line(" > Trace-back of call stack: " );
      -- Put_Line( GNAT.Traceback.Symbolic.Symbolic_Traceback(Except_ID) );
      -- See more at: http://compgroups.net/comp.lang.ada/gnat-symbolic-traceback-on-exceptions/1409155#sthash.lNdkTjq6.dpuf
      -- Do teh same manually, use:
      -- addr2line -e ./fits addr1 addr2 ...
     end;
end test;

