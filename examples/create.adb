
-- creat from User_Type data a FITS-file with MD_Raw_Type data

with Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Traceback.Symbolic;

with FITS_IO;           use FITS_IO;
--with FITS_IO.Data_Unit;
with V3_Types; use V3_Types;
with Pool_For_Numeric_Type; use Pool_For_Numeric_Type;

with Optional.Reserved; use Optional.Reserved;
--with Header;
with Card;


procedure create
is
   Zero  :  Float := 0.0;
   F_NaN : constant Float := 0.0/Zero;

   package TIO renames Ada.Text_IO;
   package TIOF renames Ada.Float_Text_IO;

   -- NOTE
   -- define Physical data -- Short_Integer example
   -- note: 32.bit Integer has 9 digits, but 16bit.Float has only 8-bit precision
   -- Undef value Int -> Float -> Int conversion chain will fail!
   -- FIXME avoid conversions!!!

  -- define Physical data -- Float example

   -- Float

--   subtype Phys_Type is Float;
--   Phys_Undef_Used  : Boolean     := True;
--   Phys_Undef_Value : Phys_Type   := F_NaN;

   -- Integer

   subtype Phys_Type is Short_Integer;
   Phys_Undef_Used  : constant Boolean   := True;
   Phys_Undef_Value : constant Phys_Type := Phys_Type'Last;

   -- Unsigned

   -- subtype Phys_Type is Unsigned_16;
   -- Phys_Undef_Used  : constant Boolean   := True;
   -- Phys_Undef_Value : constant Phys_Type := Phys_Type'Last;



   -- describe Raw data in FITS-file

   -- Unsigned 8

   -- Raw_Type      : DU_Type := FITS_IO.UInt8;
   -- Raw_Undef     : constant Integer_8 := 127;
   -- Raw_Undef_Str : constant String := Integer_8'Image(Raw_Undef);

   -- Signed 16

   -- Raw_Type      : DU_Type := FITS_IO.Int16;
   -- Raw_Undef     : constant Integer_16 := 127;
   -- Raw_Undef_Str : constant String := Integer_16'Image(Raw_Undef);


   -- Float

   Raw_Type      : DU_Type := FITS_IO.F32;
   Raw_Undef     : constant Float_32 := F32NaN;
   Raw_Undef_Str : constant String := "0";-- FIXME here with empty string fails (1* operator ??)


   -- simulate some data

   ColLength : constant Positive_Count := 127;
   RowLength : constant Positive_Count := 456;

   NAXISn : NAXIS_Array := (ColLength, RowLength);

--   package Phys_Data is new FITS_IO.Data_Unit(Phys_Type);
--   Write_Buffer : Phys_Data.T_Arr(1 .. ColLength);
   type Phys_Type_Arr is array (Positive_Count range <>) of Phys_Type;
   procedure DU_Write is new FITS_IO.HDU_Write(Phys_Type, Phys_Type_Arr);
   Write_Buffer : Phys_Type_Arr(1 .. ColLength);


   UCnt : Natural := 0;

   function Generate_Data
      (ColLength : FITS_IO.Positive_Count;
      Phys_Undef_Valid : Boolean;
      Phys_Undef_Value : Phys_Type;
      UCnt : in out Natural)
      --return Phys_Data.T_Arr
      return Phys_Type_Arr
   is
       --Col : Phys_Data.T_Arr(1 .. ColLength);
       Col : Phys_Type_Arr(1 .. ColLength);
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

begin

   TIO.Put_Line("DBG  i> "&Phys_Type'Image(Phys_Undef_Value));
   TIO.Put     ("DBG fi> "); TIOF.Put(Float(Phys_Undef_Value), 3, 10, 2);TIO.New_Line;
   TIO.Put_Line("DBGifi> "&Integer'Image(Integer(Float(Phys_Undef_Value))));


   -- Write Primary HDU

 Create (Out_File, FITS_IO.Append_File, File_Name);

 Set_Undefined_Physical(Out_File, Float(Phys_Undef_Value));
 Write_Header(Out_File, Raw_Type, NAXISn, Array_Cards);

 FITS_IO.Put_File_Type(Out_File,"DBG> ");

 for I in 1 .. RowLength
 loop
    Write_Buffer := Generate_Data(ColLength, Phys_Undef_Used, Phys_Undef_Value, UCnt);
    DU_Write(Out_File, Write_Buffer);
 end loop;

 Close(Out_File);

 TIO.Put_Line("Undefs written : " & Natural'Image(UCnt));


   -- Add extension

 Open (Out_File, FITS_IO.Append_File, File_Name);

 Set_Undefined_Physical(Out_File, Float(Phys_Undef_Value));
 Write_Header(Out_File, Raw_Type, NAXISn, Array_Cards);
 Write_Cards(Out_File, More_Cards);

 FITS_IO.Put_File_Type(Out_File,"DBG> ");

 for I in 1 .. RowLength
 loop
    Write_Buffer := Generate_Data(ColLength, Phys_Undef_Used, Phys_Undef_Value, UCnt);
    DU_Write(Out_File, Write_Buffer);
 end loop;

 Close(Out_File);


exception
  when Except_ID :
     others =>
      TIO.Put_Line(TIO.Standard_Error, Exception_Information(Except_ID));
      TIO.Put_Line(TIO.Standard_Error, GNAT.Traceback.Symbolic.Symbolic_Traceback(Except_ID));
end create;

