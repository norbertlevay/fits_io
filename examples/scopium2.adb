


with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Traceback.Symbolic;
with Ada.Streams.Stream_IO;

with FITS;
with FITS_IO.V3_Types_For_DU;
with V3_Types;
with Pool_For_Numeric_Type; use Pool_For_Numeric_Type;

with Optional;
with Optional.Reserved; use Optional.Reserved;
with Card;
with FITS_IO;

with Ada.Strings;
with Ada.Strings.Fixed;

with Debayer;

procedure scopium2
is

   package TIO renames Ada.Text_IO;
   package SIO renames Ada.Streams.Stream_IO;
   package V3T renames FITS_IO.V3_Types_For_DU;
   package FIO renames FITS_IO;


   InFileName : constant String := Argument(1);
   InFile     : SIO.File_Type;


   -- create Header
   use FITS;
   ScanLen : constant FITS.Positive_Count := 640;
   ScanCnt : constant FITS.Positive_Count := 512;
   NAXISn : FITS.NAXIS_Array := (ScanLen, ScanCnt);

   use FITS;
   Frame_Size : FITS.Positive_Count := ScanCnt * ScanLen;
   Frame : V3T.U8_Arr(1 .. Frame_Size);
   Frame_Index : FITS.Positive_Count := FITS.Positive_Count'Value(Argument(2));

   InStream : SIO.Stream_Access;
   use SIO;


   -- FITS OutFile

   procedure DU_Write is new FITS_IO.HDU_Write(V3_Types.Unsigned_8, V3T.U8_Arr);

   File_Name : constant String := InFileName & ".frame_" 
               & Ada.Strings.Fixed.Trim(
                  FITS.Positive_Count'Image(Frame_Index),Ada.Strings.Both)
              & ".fits";

   OutFile   : FITS_IO.File_Type;
   OutStream : SIO.Stream_Access;


   function Valued_Card(Key : BS_8.Bounded_String; Value : BS70.Bounded_String) return String_80
   is  
   begin
      return Card.Create_Card(BS_8.To_String(Key), BS70.To_String(Value));
   end Valued_Card;


   use Optional.BS70;
   Array_Cards : String_80_Array :=
         (Valued_Card(BZERO,    1*    "0.0"),
          Valued_Card(BSCALE,   1*    "1.0"));

   Ix_First, Ix_Last : FITS.Count;

begin

   Put_Line("Usage  " & Command_Name & "<filename.fits> <Frame_Index>");


   SIO.Open (InFile, SIO.In_File, InFileName);
   FIO.Create (OutFile, FITS_IO.Append_File, File_Name);
   FIO.Write_Header_Prim(OutFile, FITS.UInt8, NAXISn, Array_Cards);

   InStream  := SIO.Stream(InFile);
   OutStream := FIO.Stream(OutFile);

   SIO.Set_Index(InFile, 1 + SIO.Count((Frame_Index - 1) * Frame_Size) );

   V3T.U8_Arr'Read(InStream, Frame);

   Debayer.Closest_Neighbour(ScanLen, Frame);

   for I in 1 .. ScanCnt
   loop
      Ix_First := Frame'First + ScanLen * FITS.Count(I-1);
      Ix_Last  := Ix_First + ScanLen - 1;
      DU_Write(OutFile, Frame(Ix_First..Ix_Last));
   end loop;

   FIO.Close(OutFile);
   SIO.Close(InFile);

exception
   when Except_ID : others =>
      TIO.Put_Line(TIO.Standard_Error, Exception_Information(Except_ID));
      TIO.Put_Line(TIO.Standard_Error, GNAT.Traceback.Symbolic.Symbolic_Traceback(Except_ID));
end scopium2;

