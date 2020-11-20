
with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Traceback.Symbolic;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Command_Line; use Ada.Command_Line;

with V3_Types;  use V3_Types;

with File;

with Optional;
with Optional.Reserved;
with Header;
--with Buffer_Type;
with Pool_For_Numeric_Type; use Pool_For_Numeric_Type;

with Image;

with FITS_IO;
with FITS_IO.Data_Unit;

with Init;

procedure minmax is

    package TIO renames Ada.Text_IO;
    package SIO renames Ada.Streams.Stream_IO;

    Zero    : Float := 0.0;
    F_NaN   : constant Float := 0.0/Zero;

    -- access FITS-file Primary HDU
    In_File   : FITS_IO.File_Type;
    In_Stream : SIO.Stream_Access;

    -- values passed from Header to Data Read-Buffer
    Is_BLANK_In_Header  : Boolean := False;
    BLANK_Value         : Optional.BS70.Bounded_String;
    BITPIX_Value        : Integer;
    ColLength : FITS_IO.Positive_Count;
    RowLength : FITS_IO.Positive_Count;

    -- vars for 'Data Analyzes' example
    Min         : Float := Float'Last;
    Max         : Float := Float'First;
    Undef_Count : FITS_IO.Count := 0;

--        Scaling : Init.Access_Rec;
    DU : FITS_IO.Data_Unit_Type;
begin

    -- Open file as a Stream

    if(Argument_Count /= 1 ) 
    then 
      TIO.Put_Line("Usage  " & Command_Name & " <file name>");
      return;
    else
      FITS_IO.Open(In_File, FITS_IO.In_File, (Argument(1)));
      In_Stream := FITS_IO.Stream(In_File);
    end if;


    -- Read Mandatory Cards

    declare
        package F32 is new Image(Float, Float'Last);
        F32_Image : F32.Image_Rec := F32.Image_Rec'Input(In_Stream);
    begin
        BITPIX_Value := F32_Image.Target_BITPIX;
        New_Line;
        Put_Line("Target_BITPIX: " & Integer'Image(F32_Image.Target_BITPIX) );
        Put_Line("NAXIS        : " & Integer'Image(F32_Image.NAXISn'Last) );
        ColLength := F32_Image.NAXISn(1);
        if(F32_Image.NAXISn'Last > 1)
        then
            RowLength := F32_Image.NAXISn(2);
        else
            RowLength := 1;
        end if;
    end;


    -- Read Reserved Cards

    File.Set_File_Block_Index(In_File, 1);

    declare
        ResKeys : Optional.Reserved.Reserved_Key_Arr :=
            Optional.Reserved.Reserved_Key_Arr'Input(In_Stream);
        ArrKeys : Header.Valued_Key_Record_Arr(1..0);
    begin

        New_Line;
        Put_Line("ResKeys#: " & FITS_IO.Positive_Count'Image(ResKeys'Length));
        if(ResKeys'Length > 0)
        then
            for I in ResKeys'Range
            loop
                Put_Line(Optional.BS_8.To_String(ResKeys(I).Key) &" "& 
                                    Optional.BS70.To_String(ResKeys(I).Value));
                if(Optional.BS_8.To_String(ResKeys(I).Key) = "BLANK")
                then
                    BLANK_Value := ResKeys(I).Value;
                    Is_BLANK_In_Header := True;
                end if;
            end loop;
        end if;

       FITS_IO.Open(DU, In_File, FITS_IO.F32);
--        Init.Init_Reads(DUType => Init.F32, Array_Keys => ArrKeys, DU_Access => Scaling);
--        Scaling.BITPIX := 16; --F32_Image.Target_BITPIX;
--        Scaling.Undef_Used := Is_BLANK_In_Header;
--        Scaling.Undef_Raw  := -100.0;--Float'Value(Optional.BS70.To_String(BLANK_Value));
--        Scaling.Undef_Phys := F_NaN;
   end;


    -- Set-up data read buffer

    declare

        package F32_Data is new FITS_IO.Data_Unit(Float);

        -- example of data elaboration: find min max and count undef values

        subtype ColBuffer is F32_Data.Buffer(1..ColLength);

        procedure Analyze_Data
            (R : FITS_IO.Positive_Count;
            CurF32Col: ColBuffer;
            UValid : Boolean; UValue : Float)
        is
            V : Float;
            use FITS_IO;
        begin
            for I in CurF32Col'Range
            loop
                V := CurF32Col(I);
                if(UValid And (Not (V = V)))
                then
                        Undef_Count := Undef_Count + 1;
                else
                        if(V < Min)
                        then
                           Min := V;
                           TIO.Put("i" & FITS_IO.Count'Image(R) & "X"&FITS_IO.Count'Image(I));
                           TIO.Put( " " & Float'Image(V));
                        end if;
                        if(V > Max)
                        then
                           Max := V;
                           TIO.Put("x" & FITS_IO.Count'Image(R) &"X" &FITS_IO.Count'Image(I));
                           TIO.Put(" " & Float'Image(V));
                        end if;
                end if;
            end loop;
        end Analyze_Data;

        Current_F32Column : F32_Data.Buffer(1..ColLength);
        Last : FITS_IO.Count;
     begin


         -- read all Data Unit

         for I in 1 .. RowLength
         loop
             F32_Data.Read(In_File, DU, Current_F32Column, Last);
             Analyze_Data(I,Current_F32Column, True, 0.0);
                  --Scaling.Undef_Used, Scaling.Undef_Phys);
         end loop;

        -- print results

         New_Line;
--         TIO.Put_Line("Undef_Valid            : " & Boolean'Image(Scaling.Undef_Used));
--         if(Scaling.Undef_Used)
--         then
--             TIO.Put_Line("Undef_Value            : " & Float'Image(Scaling.Undef_Phys));
--         end if;
         TIO.Put_Line("Undef_Count (NaN)      : " & FITS_IO.Count'Image(Undef_Count));
         TIO.Put_Line("Min                    : " & Float'Image(Min));
         TIO.Put_Line("Max                    : " & Float'Image(Max));

     end;

    FITS_IO.Close(In_File);


exception

  when Except_ID : others =>
     declare
      Error : Ada.Text_IO.File_Type := Standard_Error;
     begin
      New_Line(Error);
      Put_Line(Error, "Exception_Information: ");
      Put_Line(Error, Exception_Information( Except_ID ) );
      New_Line(Error);
      Put_Line(Error, "Call stack traceback symbols: addr2line -e ./fits addr1 addr2 ...");
      Put_Line(" > Trace-back of call stack: " );
      Put_Line( GNAT.Traceback.Symbolic.Symbolic_Traceback(Except_ID) );
      -- See more at: http://compgroups.net/comp.lang.ada/gnat-symbolic-traceback-on-exceptions/1409155#sthash.lNdkTjq6.dpuf
      -- Do the same manually, use:
      -- addr2line -e ./fits addr1 addr2 ...
     end;

end minmax;

