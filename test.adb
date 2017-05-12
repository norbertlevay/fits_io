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
with HDU;


procedure test
is
 Fits : Fits_IO.File_Type;
 Mode : Fits_IO.File_Mode := Append_File;
 Name : String    := "test.fits";
 Header : HDU.Header_Type :=
  (HDU.SB.To_Bounded_String("NAXIS   = 2"),
   HDU.SB.To_Bounded_String("NAXIS1  = 10"),
   HDU.SB.To_Bounded_String("NAXIS2  = 10"),
   HDU.SB.To_Bounded_String("END"));
begin

 Create (Fits, Mode, Name);

 Write (Fits, Header);

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

