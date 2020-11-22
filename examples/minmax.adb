
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
with Pool_For_Numeric_Type; use Pool_For_Numeric_Type;

with Image;

with FITS_IO; use FITS_IO;
with FITS_IO.Data_Unit;

with Init;

procedure minmax is

    package TIO renames Ada.Text_IO;
    package SIO renames Ada.Streams.Stream_IO;

    Zero    : Float := 0.0;
    F_NaN   : constant Float := 0.0/Zero;

    In_File   : FITS_IO.File_Type;

    -- values passed from Header to Data Read-Buffer
    Is_BLANK_In_Header  : Boolean := False;
    BLANK_Value         : String(1..20);--Optional.BS70.Bounded_String;
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

   -- Open file

   if(Argument_Count /= 1 ) 
   then 
      TIO.Put_Line("Usage  " & Command_Name & " <file name>");
      return;
   else
      FITS_IO.Open(In_File, FITS_IO.In_File, (Argument(1)));
   end if;


   -- Read Header

   declare
      Image : FITS_IO.Image_Rec := Read_Header(In_File, Optional.Reserved.Array_Keys);
   begin
      BITPIX_Value := Image.BITPIX;
      New_Line;
      Put_Line("BITPIX : " & Integer'Image(Image.BITPIX) );
      Put_Line("NAXIS  : " & Integer'Image(Image.NAXISn'Last) );

      for I in Image.Array_Keys'Range
      loop
         Put_Line(">"&Image.Array_Keys(I)&"<");
         if(Image.Array_Keys(I)(1..5) = "BLANK")
         then
            Is_BLANK_In_Header := True;
            BLANK_Value := Image.Array_Keys(I)(11..30);
         end if;
        end loop;

        ColLength := Image.NAXISn(1);
        if(Image.NAXISn'Last > 1)
        then
            RowLength := Image.NAXISn(2);
        else
            RowLength := 1;
        end if;
    end;

    -- Set-up data read buffer

    FITS_IO.Open(DU, In_File, FITS_IO.Int16);
    -- FIXME Int16 param not needed REVIEW

    declare

        package F32_Data is new FITS_IO.Data_Unit(Float);

        -- example of data elaboration: find min max and count undef values

        subtype ColBuffer is F32_Data.T_Arr(1..ColLength);

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
                        if(V < Min) then Min := V; end if;
                        if(V > Max) then Max := V; end if;
                end if;
            end loop;
        end Analyze_Data;

        Current_F32Column : F32_Data.T_Arr(1..ColLength);
        Last : FITS_IO.Count;
     begin


         -- read all Data Unit

         for I in 1 .. RowLength
         loop
             F32_Data.Read(In_File, DU, Current_F32Column, Last);
             Analyze_Data(I,Current_F32Column, True, 0.0);
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

    FITS_IO.Close(DU,In_File);
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

