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
	    Options : String;
            HDUNum : Positive) is separate;



 InFileName : SU.Unbounded_String; 
 InFile     : SIO.File_Type;
-- InOptions  : SU.Unbounded_String;
 CurHDU : Positive := 3;

begin
 
 --if (CLI.Argument_Count /= 2) then
 if (CLI.Argument_Count /= 1) then
   --TIO.Put_Line("Usage  " & CLI.Command_Name & " <file name> <options>");
   TIO.Put_Line("Usage  " & CLI.Command_Name & " <file name>");
   --TIO.Put_Line("         options : NONE | MAND | RES");
   return;
 else
   InFileName := SU.To_Unbounded_String (CLI.Argument (1));
 --  InOptions  := SU.To_Unbounded_String (CLI.Argument (2));
 end if;

--  TIO.Unbounded_IO.Put_Line(InFileName);
--  TIO.Unbounded_IO.Put_Line(InOptions);


  SIO.Open(InFile, SIO.In_File, SU.To_String(InFileName));

  Ada.Text_IO.Put_Line("Set curHDU: "& Positive'Image(CurHDU));
  main.Set_Index(InFile,"not_used",CurHDU);
  --main.Set_Index(InFile,SU.To_String(InOptions),CurHDU);

  SIO.Close(InFile);


end main;
