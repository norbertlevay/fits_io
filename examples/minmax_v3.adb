
    -- FIXME Stream may be repositioned only outside the operators
    -- to do Mandatory+Reserved Keys in one call: mandatory parser needs to
    -- be enhanced to parse also Reserved-Keys and return in Reserved.Get 
    -- (Similar to Mandatory.Get)




with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Traceback.Symbolic;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Command_Line; use Ada.Command_Line;

with V3_Types;  use V3_Types;

with File;
--with DU_Type.V3_Image_Read;

with Optional;
with Optional.Reserved;
with Header;
with Buffer_Type;
with Pool_For_Numeric_Type; use Pool_For_Numeric_Type;
--with V3_Pool_Scaling;  use V3_Pool_Scaling;

with Image;

procedure minmax_V3 is

    package TIO renames Ada.Text_IO;
    package SIO renames Ada.Streams.Stream_IO;

    Zero    : Float := 0.0;
    F_NaN   : constant Float := 0.0/Zero;

    -- access FITS-file Primary HDU
    In_File   : SIO.File_Type;
    In_Stream : SIO.Stream_Access;
    HDUStart  : constant SIO.Positive_Count := 1;

    -- values passed from Header to Data Read-Buffer
    Is_BLANK_In_Header  : Boolean := False;
    BLANK_Value         : Optional.BS70.Bounded_String;
    BITPIX_Value        : Integer;
    ColLength : SIO.Positive_Count;
    RowLength : SIO.Positive_Count;

    -- vars for 'Data Analyzes' example
    Min         : Float := Float'Last;
    Max         : Float := Float'First;
    Undef_Count : SIO.Count := 0;

begin

    -- Open file as a Stream

    if(Argument_Count /= 1 ) 
    then 
      TIO.Put_Line("Usage  " & Command_Name & " <file name>");
      return;
    else
      SIO.Open(In_File, SIO.In_File, (Argument(1)));
      In_Stream := SIO.Stream(In_File);
    end if;


    -- Read Mandatory Cards

    File.Set_File_Block_Index(In_File, HDUStart);

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

    File.Set_File_Block_Index(In_File, HDUStart);

    declare
        ResKeys : Optional.Reserved.Reserved_Key_Arr := 
            Optional.Reserved.Reserved_Key_Arr'Input(In_Stream);
    begin
        New_Line;
        Put_Line("ResKeys#: " & SIO.Positive_Count'Image(ResKeys'Length));
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
    end;


    -- Set-up data read buffer

    declare
        Memory_Undefined_Valid  : Boolean := Is_BLANK_In_Header;--File_Undefined_Valid;
        Memory_Undefined_Value  : Float   := F_NaN;   -- FIXME calc not set
        -- FIXME above needed in Analyze_Data (and print results)

        package F32_Data is new Buffer_Type
            (T => Float,
            Memory_Undefined_Value  => F_NaN,
            Memory_Undefined_Valid  => Is_BLANK_In_Header,
            File_Undefined_Value    => Float'Value(Optional.BS70.To_String(BLANK_Value)),
            File_Undefined_Valid    => Is_BLANK_In_Header,
            A => 0.0,  -- FIXME calc, not set
            B => 1.0,  -- FIXME calc, not set
            Memory_BITPIX   => -(Float'Size), -- from Data Model
            File_BITPIX     => BITPIX_Value); -- from Header


        -- example of data elaboration: find min max and count undef values

        subtype ColBuffer is F32_Data.Buffer(1..ColLength);

        procedure Analyze_Data
            (R : SIO.Positive_Count;
            CurF32Col: ColBuffer;
            UValid : Boolean; UValue : Float)
        is
            V : Float;
            use SIO;
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

        Current_F32Column : F32_Data.Buffer(1..ColLength);
 
     begin

         -- read all Data Unit

         for I in 1 .. RowLength
         loop
             F32_Data.Buffer'Read(In_Stream, Current_F32Column);
             Analyze_Data(I,Current_F32Column, True, Memory_Undefined_Value);
         end loop;

        -- print results

         New_Line;
         TIO.Put_Line("Undef_Valid            : " & Boolean'Image(Memory_Undefined_Valid));
         if(Memory_Undefined_Valid)
         then
             TIO.Put_Line("Undef_Value            : " & Float'Image(Memory_Undefined_Value));
         end if;
         -- TIO.Put_Line("Special_Count (Inf...) : " & SIO.Count'Image(Special_Count));
         TIO.Put_Line("Undef_Count (NaN)      : " & SIO.Count'Image(Undef_Count));
         TIO.Put_Line("Min                    : " & Float'Image(Min));
         TIO.Put_Line("Max                    : " & Float'Image(Max));

     end;

    SIO.Close(In_File);


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

end minmax_V3;

