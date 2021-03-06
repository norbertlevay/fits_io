--
-- Example list info on each HDU in FITS-file
--
-- FIXME temp: for testing of FITS_IO (DUSize calc)
--


with Ada.Text_IO;      --use Ada.Text_IO;
with Ada.Command_Line; --use Ada.Command_Line;

with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Traceback.Symbolic;

with Ada.Streams.Stream_IO;  --use Ada.Streams.Stream_IO;


with File;   use File;
with Ada.Strings.Unbounded;

with Header; use Header;
with Optional; --use Keyword_Record;
with Optional.Reserved;   use Optional.Reserved;

procedure reserved_keys
is

 package TIO renames Ada.Text_IO;
 package SIO renames Ada.Streams.Stream_IO;
 package CLI renames Ada.Command_Line;
 package SU  renames Ada.Strings.Unbounded;

 InFileName : SU.Unbounded_String; 
 InFile     : SIO.File_Type;
 HDUNum     : Positive := 1;

begin

 if (CLI.Argument_Count /= 1) then
   TIO.Put_Line("Usage  " & CLI.Command_Name & " <file name>");
   return;
 else
   InFileName := SU.To_Unbounded_String (CLI.Argument (1));
 end if;



   SIO.Open(InFile, SIO.In_File, SU.To_String(InFileName));
	
   while not SIO.End_Of_File(InFile)
   loop
     declare
       Cards : Optional.Card_Arr := Read_Optional(InFile, Optional.Reserved.Reserved_Keys);
     begin
       TIO.Put_Line("RESKEYS: HDU# " & Positive'Image(HDUNum));
	for I in Cards'Range
	loop
		TIO.Put_Line("RESKEYS: >" & Cards(I) & "<" );
	end loop;


       HDUNum := HDUNum + 1;
       Set_Index(InFile, HDUNum);
     end;
   end loop;

   SIO.Close(InFile);



 exception

  when Except_ID : others =>
     declare
      Error : TIO.File_Type := TIO.Standard_Error;
     begin
      TIO.Put_Line(Error, Exception_Message( Except_ID ) );
      TIO.New_Line(Error);
      TIO.Put_Line(Error, Exception_Information( Except_ID ) );
      TIO.Put_Line(Error, "Call stack traceback symbols: addr2line -e ./list -a addr1 addr2 ...");

      -- TIO.Put_Line(Error, " > Trace-back of call stack: " );
      -- TIO.Put_Line(Error, GNAT.Traceback.Symbolic.Symbolic_Traceback(Except_ID) );
      -- See more at: http://compgroups.net/comp.lang.ada/gnat-symbolic-traceback-on-exceptions/1409155#sthash.lNdkTjq6.dpuf
      -- Do the same manually, use:
      -- addr2line -e ./fits -a addr1 addr2 ...
     end;
end reserved_keys;

   
   


