--
-- Usage:  scopium-raw <file.dat> <i>
--
-- Extracts i-th frame (640x513) from raw-video taken with this command:
--
-- sudo ./src/scopium -g 15 -x 100 -n 3940 --streaming --rgb24 --intensity  2>/dev/null \
-- |mplayer -rawvideo fps=30:w=640:h=513:format=rgb24 -demuxer rawvideo - 
--
-- Then use ImageMagick 'convert' command to create png-file
--
-- convert -depth 8 -size 640x513+0 rgb:<frame_i.raw> out.png
--
-- NOTE: raw-video is concatinated sequence of rgb24 frames, without any gap
-- one pixel:              3 bytes (RGB stored in consecutive locations)
-- one scan-line length:   3*640
-- one frames size is:     3*640 x 513
--

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Traceback.Symbolic;
with Ada.Streams.Stream_IO;

with Ada.Strings;
with Ada.Strings.Fixed;

with Interfaces;

procedure scopium_raw
is

   package TIO renames Ada.Text_IO;
   package SIO renames Ada.Streams.Stream_IO;
   package STR renames Ada.Strings;

   -- InFile

   InFileName  : constant String := Argument(1);
   InFile      : SIO.File_Type;
   InStream    : SIO.Stream_Access;

   -- OutFile

   Frame_Index : SIO.Positive_Count := SIO.Positive_Count'Value(Argument(2));
   File_Name : constant String
               := InFileName & ".frame_" 
               & STR.Fixed.Trim(SIO.Positive_Count'Image(Frame_Index), STR.Both)
               & ".raw";

   OutFile  : SIO.File_Type;
   OutStream : SIO.Stream_Access;

   -- Frame buffer

   use SIO;
   ScanLen     : constant SIO.Positive_Count := 640*3;
   ScanCnt     : constant SIO.Positive_Count := 513;
   Frame_Size  : constant SIO.Positive_Count := ScanCnt * ScanLen;


   subtype UInt8 is Interfaces.Unsigned_8;
   type Frame_Type is array (SIO.Positive_Count range 1 .. Frame_Size) of UInt8;
   Frame : Frame_Type;

   Ix_First, Ix_Last : SIO.Count;

begin

   Put_Line("Usage  " & Command_Name & "<filename.fits> <Frame_Index>");

   SIO.Open   (InFile,  SIO.In_File,     InFileName);
   Put_Line(InFileName & " contains "
         &  SIO.Count'Image(SIO.Size(InFile) / Frame_Size) & " RGB24-frames");

   SIO.Create (OutFile, SIO.Append_File, File_Name);
   InStream  := SIO.Stream(InFile);
   OutStream := SIO.Stream(OutFile);

   SIO.Set_Index(InFile, 1 + SIO.Positive_Count((Frame_Index - 1) * Frame_Size));

   Frame_Type'Read (InStream,  Frame);
   Frame_Type'Write(OutStream, Frame);

   SIO.Close(OutFile);
   SIO.Close(InFile);

exception
   when Except_ID : others =>
      TIO.Put_Line(TIO.Standard_Error, Exception_Information(Except_ID));
      TIO.Put_Line(TIO.Standard_Error, GNAT.Traceback.Symbolic.Symbolic_Traceback(Except_ID));
end scopium_raw;

