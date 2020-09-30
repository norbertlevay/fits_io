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


procedure minmax_V3 is

    package TIO renames Ada.Text_IO;
    package SIO renames Ada.Streams.Stream_IO;

    InFile   : SIO.File_Type;
    In_Stream : SIO.Stream_Access;
    HDUStart : SIO.Positive_Count := 1; -- Primary HDU only

begin

    if(Argument_Count /= 1 ) 
    then 
      TIO.Put_Line("Usage  " & Command_Name & " <file name>");
      return;
    else
      SIO.Open(InFile, SIO.In_File, (Argument(1)));
      In_Stream := SIO.Stream(InFile);
    end if;

    File.Set_File_Block_Index(InFile,HDUStart);

    declare
        HDUInfo : File.HDU_Info_Type := File.Read_Header(InFile);
    begin
        File.Set_File_Block_Index(InFile,HDUStart);
        declare
            Cards   : Optional.Card_Arr := 
                Header.Read_Optional(InFile, Optional.Reserved.Array_Keys);

            Array_Cards : Optional.Card_Arr := Optional.Find_Key(Cards, Optional.Reserved.BLANK);

            -- FIXME cards read -> all info available convert it!
                -- Mandatory in HDUInfo
                -- all Reserved-keys in Cards, if present in Header

            ColLength : SIO.Positive_Count := HDUInfo.NAXISn(1);
            RowLength : SIO.Positive_Count := HDUInfo.NAXISn(2);
            -- use to set-up Buffer size

            use Optional;
            Zero    : Float := 0.0;
            F_NaN   : constant Float := 0.0/Zero;

            -- set-up data-transfer buffer for Read

            File_Undefined_Valid    : Boolean   := Not (Array_Cards = Optional.Null_Card_Arr);
            File_Undefined_Value    : Float     := Float'Value(Array_Cards(1)(11..30));
            Memory_Undefined_Valid  : Boolean   := File_Undefined_Valid;
            Memory_Undefined_Value  : Float     := F_NaN;   -- FIXME calc type-independently
            A                       : Float     := 0.0;     -- FIXME
            B                       : Float     := 1.0;     -- FIXME
            Memory_BITPIX           : Integer   := -32;             -- from DataModel
            File_BITPIX             : Integer   := HDUInfo.BITPIX;  -- from Header

            package F32_Data is new Buffer_Type(Float, 
                    Memory_Undefined_Value, Memory_Undefined_Valid,
                    File_Undefined_Value,   File_Undefined_Valid,
                    A,B,
                    Memory_BITPIX,
                    File_BITPIX);

            subtype ColBuffer is F32_Data.Buffer(1..ColLength);
            Current_F32Column : F32_Data.Buffer(1..ColLength);

            -- example of data elaboration: find min max and count undef values
            Min : Float := Float'Last;
            Max : Float := Float'First;
            Undef_Count : SIO.Count := 0;

            procedure Analyze_Data(R : SIO.Positive_Count;
                                CurF32Col : ColBuffer;
                                UValid : Boolean; UValue : Float)
            is
                V : Float;
                use SIO;
            begin

                for I in CurF32Col'Range
                loop

                    V := CurF32Col(I);
                    --TIO.Put(" "&Float'Image(V));
                    if(UValid)
                    then
                       -- TIO.Put_Line("DBG1" & Float'Image(V));
                       if((Not (V = V)))
                       then
                           -- TIO.Put_Line("DBG2");
                           Undef_Count := Undef_Count + 1;
                       else
                           if(V < Min) then Min := V; end if;
                           if(V > Max) then Max := V; end if;
                       end if;
                    else
                        if(V < Min) then Min := V; end if;
                        if(V > Max) then Max := V; end if;
                    end if;
              end loop;
            end Analyze_Data;


        -- main --------------------------------------------------------------------
        begin

            for I in 1 .. RowLength
            loop

                F32_Data.Buffer'Read(In_Stream, Current_F32Column);

                Analyze_Data(I,Current_F32Column, True, Memory_Undefined_Value);

            end loop;



            TIO.Put_Line("Undef_Valid            : " & Boolean'Image(Memory_Undefined_Valid));
            if(Memory_Undefined_Valid)
            then
                TIO.Put_Line("Undef_Value            : " & Float'Image(Memory_Undefined_Value));
            end if;
--            TIO.Put_Line("Special_Count (Inf...) : " & SIO.Count'Image(Special_Count));
            TIO.Put_Line("Undef_Count (NaN)      : " & SIO.Count'Image(Undef_Count));
            TIO.Put_Line("Min                    : " & Float'Image(Min));
            TIO.Put_Line("Max                    : " & Float'Image(Max));
            
            TIO.Put_Line("Optional.Reserved cards:");
            for I in Cards'Range
            loop
                Put_Line(Cards(I));
            end loop;


        end;
    end;


    SIO.Close(InFile);

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

