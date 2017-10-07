                                       -- Chapter 24 - Program 3
with Ada.Text_IO, Ada.Integer_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO;
with Ada.Direct_IO;

procedure BiRandIO is

   type MY_REC is
      record
         Age     : INTEGER;
         Sex     : CHARACTER;
         Initial : CHARACTER;
      end record;

   package Ran_IO is new Ada.Direct_IO(MY_REC);
   use Ran_IO;

   Myself     : MY_REC;
   My_In_Out_File : Ran_IO.FILE_TYPE;

   procedure Display_Record(In_Rec : MY_REC) is
   begin
      Put("Record number");
      Put(In_Rec.Age, 4);
      Put(" ");
      Put(In_Rec.Sex);
      Put(" ");
      Put(In_Rec.Initial);
      New_Line;
   end Display_Record;

begin

   Open(My_In_Out_File, InOut_File, "NAMEFILE.TXT");

   Read(My_In_Out_File, Myself, 37);
   Display_Record(Myself);
   Read(My_In_Out_File, Myself, 25);
   Display_Record(Myself);
   Read(My_In_Out_File, Myself);
   Display_Record(Myself);
   New_Line;

   Myself.Age := 33;
   Myself.Sex := 'F';
   Myself.Initial := 'Z';
   Write(My_In_Out_File, Myself, 91);
   Write(My_In_Out_File, Myself, 96);
   Write(My_In_Out_File, Myself);

   Set_Index(My_In_Out_File, 88);
   while not End_Of_File(My_In_Out_File) loop
      Read(My_In_Out_File, Myself);
      Display_Record(Myself);
   end loop;

   Close(My_In_Out_File);

end BiRandIO;




-- Result of Execution

-- Record number  37 M X
-- Record number  25 M X
-- Record number  26 M X

-- Record number  88 M X
-- Record number  89 M X
-- Record number  90 M X
-- Record number  33 F Z
-- Record number  92 M X
-- Record number  93 M X
-- Record number  94 M X
-- Record number  95 M X
-- Record number  33 F Z
-- Record number  33 F Z
-- Record number  98 M X
-- Record number  99 M X
-- Record number 100 M X

