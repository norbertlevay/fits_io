-- NOTE
-- define Physical data -- Short_Integer example
-- note: 32.bit Integer has 9 digits, but 16bit.Float has only 8-bit precision
-- Undef value Int -> Float -> Int conversion chain will fail!
-- FIXME avoid conversions!!!


-- create from User_Type data a FITS-file with MD_Raw_Type data

with Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Traceback.Symbolic;
with Ada.Streams;
with FITS_IO;  use FITS_IO;
with V3_Types; use V3_Types;

with Optional.Reserved; use Optional.Reserved;
with Card;

with FITS;
-- FIXME remove FITS later when module's type dependencies resolved

procedure create
is
   Zero  :  Float := 0.0;
   F_NaN : constant Float := 0.0/Zero;

   package TIO renames Ada.Text_IO;
   package TIOF renames Ada.Float_Text_IO;

   -- Physical data

   -- Float

   -- subtype Phys_Type is Float_32;
   -- Phys_Undef_Used  : Boolean     := True;
   -- Phys_Undef_Value : Phys_Type   := F32NaN;
   -- subtype Phys_Type_Arr is F32_Arr;

   -- Integer

   subtype Phys_Type is Integer_16;
   Phys_Undef_Used  : constant Boolean   := True;
   Phys_Undef_Value : constant Phys_Type := Phys_Type'Last;
   subtype Phys_Type_Arr is I16_Arr;

   -- Unsigned

   -- subtype Phys_Type is Unsigned_16;
   -- Phys_Undef_Used  : constant Boolean   := True;
   -- Phys_Undef_Value : constant Phys_Type := Phys_Type'Last;
   -- subtype Phys_Type_Arr is U8_Arr;


   -- Raw data (in FITS-file)

   -- Unsigned

   -- Raw_Type      : DU_Type := FITS_IO.UInt8;
   -- Raw_Undef     : constant Integer_8 := 127;
   -- Raw_Undef_Str : constant String := Integer_8'Image(Raw_Undef);

   -- Signed

   -- Raw_Type      : DU_Type := FITS_IO.Int16;
   -- Raw_Undef     : constant Integer_16 := 127;
   -- Raw_Undef_Str : constant String := Integer_16'Image(Raw_Undef);


   -- Float

   Raw_Type      : DU_Type := FITS.F32;
   Raw_Undef     : constant Float_32 := F32NaN;
   Raw_Undef_Str : constant String := "0";-- FIXME here with empty string fails (1* operator ??)

   -- create some data

   ColLength : constant Positive_Count := 127;
   RowLength : constant Positive_Count := 456;

   NAXISn : NAXIS_Array := (ColLength, RowLength);

   -- procedure DU_Write is new FITS_IO.HDU_Write(Phys_Type, Phys_Type_Arr);
   Write_Buffer : Phys_Type_Arr(1 .. ColLength);


   UCnt : Natural := 0;

   function Generate_Data
      (ColLength : FITS_IO.Positive_Count;
      Phys_Undef_Valid : Boolean;
      Phys_Undef_Value : Phys_Type;
      UCnt : in out Natural)
      return Phys_Type_Arr
   is
      Col : Phys_Type_Arr(1 .. ColLength);
      use FITS;-- FIXME remove later
   begin
      for I in Col'Range loop Col(I) := Phys_Type(I - 1); end loop;
      if (Phys_Undef_Valid)
      then
         Col(ColLength/2) := Phys_Undef_Value;
         UCnt := UCnt + 1;
      end if;
      return Col;
   end Generate_Data;


   -- build Header

   function Valued_Card(Key : BS_8.Bounded_String; Value : BS70.Bounded_String) return String_80
   is  
   begin
      return Card.Create_Card(BS_8.To_String(Key), BS70.To_String(Value));
   end Valued_Card;

   use Optional.BS70;
   Array_Cards : String_80_Array :=
      (Valued_Card(BZERO,    1*    "0.0"),
   Valued_Card(BSCALE,   1*    "1.0"));
   --                Valued_Card(BLANK,    1*    Raw_Undef_Str));-- FIXME why need off if string ""
   --                Valued_Card(DATAMIN,  1*    "0.0"),
   --                Valued_Card(DATAMAX,  1*  "126.0"));
   -- FIXME above cards must have calculated value

   More_Cards : String_80_Array :=
      (Valued_Card(DATAMIN,  1*    "0.0"),
   Valued_Card(DATAMAX,  1*  "126.0"));


   File_Name : constant String := Command_Name & ".fits";
   Out_File  : FITS_IO.File_Type;
   Out_Stream : access Ada.Streams.Root_Stream_Type'Class;

begin
   -- Write Primary HDU

   Create (Out_File, FITS_IO.Append_File, File_Name);
   Out_Stream := FITS_IO.HDU_Stream(Out_File);

   Set_Undefined_Physical(Out_File, Float(Phys_Undef_Value));
   Write_Header_Prim(Out_File, Raw_Type, NAXISn, Array_Cards);

   FITS_IO.Put_File_Type(Out_File,"DBG> ");

   for I in 1 .. RowLength
   loop
      Write_Buffer := Generate_Data(ColLength, Phys_Undef_Used, Phys_Undef_Value, UCnt);
      --DU_Write(Out_File, Write_Buffer);
      TIO.New_Line;
      TIO.Put(FITS_IO.Count'Image(I));
      Phys_Type_Arr'Write(Out_Stream, Write_Buffer);
   end loop;

   Close(Out_File);

   TIO.Put_Line("Undefs written : " & Natural'Image(UCnt));


   -- Add extension

   Open (Out_File, FITS_IO.Append_File, File_Name);
   Out_Stream := FITS_IO.HDU_Stream(Out_File);

   Set_Undefined_Physical(Out_File, Float(Phys_Undef_Value));
   Write_Header_Ext(Out_File, Raw_Type, NAXISn, Array_Cards);
   Write_Cards(Out_File, More_Cards);

   FITS_IO.Put_File_Type(Out_File,"DBG> ");

   for I in 1 .. RowLength
   loop
      Write_Buffer := Generate_Data(ColLength, Phys_Undef_Used, Phys_Undef_Value, UCnt);
      --DU_Write(Out_File, Write_Buffer);
      Phys_Type_Arr'Write(FITS_IO.HDU_Stream(Out_File), Write_Buffer);
   end loop;

   Close(Out_File);


exception
   when Except_ID :
      others =>
      TIO.Put_Line(TIO.Standard_Error, Exception_Information(Except_ID));
      TIO.Put_Line(TIO.Standard_Error, GNAT.Traceback.Symbolic.Symbolic_Traceback(Except_ID));
end create;

