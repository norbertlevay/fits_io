
-- creat from User_Type data a FITS-file with MD_Raw_Type data

with Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Traceback.Symbolic;

with FITS_IO;           use FITS_IO;
with FITS_IO.Data_Unit;
with Pool_For_Numeric_Type; use Pool_For_Numeric_Type;

with Optional.Reserved; use Optional.Reserved;
with Header;
with Image;



procedure create
is

    Zero  :  Float := 0.0;
    F_NaN : constant Float := 0.0/Zero;

    package TIO renames Ada.Text_IO;

    subtype User_Type is Short_Short_Integer;
    MD_Raw_Type : DU_Type := FITS_IO.UInt8;

   -- Metadata

    ColLength : constant Positive_Count := 127;
    RowLength : constant Positive_Count := 456;

    MD_NAXISn      : NAXIS_Array := (ColLength, RowLength);
    Simulate_Undef : Boolean   := True;
    MD_Undef_Value : Float     := Float(User_Type'Last);--F_NaN;

    HDU_First_Card : String_80_Array(1 .. 1) := 
      (1 => Header.Create_Mandatory_Card("SIMPLE", Header.To_Value_String(True)));

    use Optional.BS70;
    Array_Cards : String_80_Array :=
               (Valued_Card(BZERO,    1*    "0.0"),
                Valued_Card(BSCALE,   1*    "1.0"),
                Valued_Card(BLANK,    1*     Integer'Image(Integer(MD_Undef_Value))),
                Valued_Card(DATAMIN,  1*    "0.0"),
                Valued_Card(DATAMAX,  1*  "126.0"));
    -- FIXME above cards must have calculated value

   -- simulate some data

    -- FIXME works only for Float, for Ints raises overflow excpetion at conv ABFloat->Int
   package User_Data is new FITS_IO.Data_Unit(User_Type);

   UCnt : Natural := 0;

   function Generate_Data
      (ColLength : FITS_IO.Positive_Count;
      Undef_Valid : Boolean;
      Undef_Value : Float)
      return User_Data.T_Arr
   is
       Col : User_Data.T_Arr(1 .. ColLength);
   begin
       for I in Col'Range loop Col(I) := User_Type(I - 1); end loop;
       if (Undef_Valid)
       then
          Col(ColLength/2) := User_Type(Undef_Value);
          UCnt := UCnt + 1;
       end if;
       return Col;
   end Generate_Data;

   File_Name : constant String := Command_Name & ".fits";
   Out_File  : FITS_IO.File_Type;

   Buffer : User_Data.T_Arr(1 .. ColLength);

begin

 Create (Out_File, FITS_IO.Append_File, File_Name);

 -- write Header and Data unit

 Write(Out_File, HDU_First_Card);

 Set_Undefined_Physical(Out_File, MD_Undef_Value);
 Write_Header(Out_File, MD_Raw_Type, MD_NAXISn, Array_Cards);

 FITS_IO.Put_File_Type(Out_File,"DBG> ");

 for I in 1 .. RowLength
 loop
    Buffer := Generate_Data(ColLength, Simulate_Undef,MD_Undef_Value);
--    for I in Buffer'Range loop TIO.Put(" "&Float'Image(Buffer(I))); end loop;
    User_Data.Write(Out_File, Buffer);
 end loop;

 Write_Data_Padding(Out_File);

 Close(Out_File);

   TIO.Put_Line("Undefs written : " & Natural'Image(UCnt));


exception
  when Except_ID :
     others =>
      TIO.Put_Line(TIO.Standard_Error, Exception_Information(Except_ID));
      TIO.Put_Line(GNAT.Traceback.Symbolic.Symbolic_Traceback(Except_ID));
      -- for Tracenack buil with:
      -- gnatmake -Pexamples create -bargs -E
end create;

