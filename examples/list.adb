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

with Ada.Strings.Unbounded;

with FITS_IO.File;   --use FITS_IO.File;
with FITSlib.File;   --use FITSlib.File;
with FITSlib.Header;   use FITSlib.Header; -- HDU_Variant needed

procedure list
is

 package TIO renames Ada.Text_IO;
 package SIO renames Ada.Streams.Stream_IO;
 package CLI renames Ada.Command_Line;
 package SU  renames Ada.Strings.Unbounded;
 package FIO renames FITS_IO.File;

 InFileName : SU.Unbounded_String; 
 InFile     : SIO.File_Type;
 HDUNum     : Positive := 1;
 DSize      : Natural := 0;
 DUSize     : Natural := 0;
 DURem      : Natural; 
begin

 if (CLI.Argument_Count /= 1) then
   TIO.Put_Line("Usage  " & CLI.Command_Name & " <file name>");
   return;
 else
   InFileName := SU.To_Unbounded_String (CLI.Argument (1));
 end if;



   SIO.Open(InFile, SIO.In_File, SU.To_String(InFileName));

   -- type
   TIO.Put_Line("HDU: " &
     HDU_Variant'Image( FITSlib.File.Peek(InFile)  )
   );


   FITSlib.File.Read_HDU(InFile);

   FIO.Set_Index(InFile, 1);

   -- Peek() resets origi FileIndex: no need for this: FIO.Set_Index(InFile, 1);
   -- size
   DSize := FITSlib.File.Read_DataSize_bits(InFile);
   TIO.Put_Line("Data Size     [bytes] : " & Natural'Image(DSize/8));
   DURem  := (DSize/8) rem 2880;
   TIO.Put_Line("DURem : " & Natural'Image(DURem));
   DUSize := ((DSize/8) / 2880 + 1) * 2880;
   if(DURem = 0) then
	   DUSize := DUSize - 2880;
   end if;
   TIO.Put_Line("DataUnit Size [bytes] : " & Natural'Image(DUSize));
   
   FIO.Set_Index(InFile, 1);
   while not SIO.End_Of_File(InFile)
   loop
     declare
       -- FIXME tbd later: HDUInfo : HDU_Info := FITS_IO.File.List.Get(InFile);
     begin
       TIO.Put_Line(Positive'Image(HDUNum));
       HDUNum := HDUNum + 1;
       FIO.Set_Index(InFile, HDUNum);
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
end list;

   
   


