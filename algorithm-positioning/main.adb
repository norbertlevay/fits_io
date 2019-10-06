with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
-------------------------------



procedure main 
is

 package TIO renames Ada.Text_IO;
 package SIO renames Ada.Streams.Stream_IO;
 use SIO;
 package CLI renames Ada.Command_Line;
 package SU  renames Ada.Strings.Unbounded;

 procedure Set_Index
           (File   : SIO.File_Type;
            HDUNum : Positive) is separate;



 InFileName : SU.Unbounded_String; 
 InFile     : SIO.File_Type;

begin
 
 if (CLI.Argument_Count /= 1) then
   TIO.Put_Line("Usage  " & CLI.Command_Name & " <file name>");
   return;
 else
   InFileName := SU.To_Unbounded_String (CLI.Argument (1));
 end if;

  TIO.Unbounded_IO.Put_Line(InFileName);


  SIO.Open(InFile, SIO.In_File, SU.To_String(InFileName));

  main.Set_Index(InFile,3);

  SIO.Close(InFile);


end main;
