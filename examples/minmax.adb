
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
--with FITS_IO.Data_Unit;

with Init;

procedure minmax is

    package TIO renames Ada.Text_IO;
    package SIO renames Ada.Streams.Stream_IO;

   subtype Phys_Type is Long_Long_Float;

    Zero    : Float := 0.0;
    F_NaN   : constant Float := 0.0/Zero;

    In_File   : FITS_IO.File_Type;

    -- values passed from Header to Data Read-Buffer
    Is_BLANK_In_Header  : Boolean := False;
    BLANK_Value         : String(1..20);
    -- in case Header has BLANK use this Undef for Phys:
    Undef_Phys_Valid    : Boolean := True;
    Undef_Phys : constant Float := F_NaN;

    ColLength : FITS_IO.Positive_Count;
    RowLength : FITS_IO.Positive_Count;

    -- vars for 'Data Analyzes' example
    Min         : Phys_Type := Phys_Type'Last;
    Max         : Phys_Type := Phys_Type'First;
    Undef_Count : FITS_IO.Count := 0;

begin

   -- Open file

   if(Argument_Count /= 1 ) 
   then 
      TIO.Put_Line("Usage  " & Command_Name & " <file name>");
      return;
   else
      Open(In_File, FITS_IO.In_File, (Argument(1)));
   end if;

   Set_Undefined_Physical(In_File, Undef_Phys);
   -- we expect undef values in Data and want to use own Undef

   -- Read Header

   declare
      Image : FITS_IO.Image_Rec := Read_Header(In_File, Optional.Reserved.Array_Keys);
   begin
      New_Line;
      FITS_IO.Put_File_Type(In_File, "DBG> ");
      New_Line;
      Put_Line("DU_Type : " & FITS_IO.DU_Type'Image(Image.Data_Type) );
      Put_Line("NAXIS   : " & Integer'Image(Image.NAXISn'Last) );

      for I in Image.Image_Cards'Range
      loop
         Put_Line(">"&Image.Image_Cards(I)&"<");
         if(Image.Image_Cards(I)(1..5) = "BLANK")
         then
            Is_BLANK_In_Header := True;
            BLANK_Value := Image.Image_Cards(I)(11..30);
            --Set_Undefined_Values(In_File,Float'Value(BLANK_Value), Undef_Phys);
         end if;
        end loop;

        ColLength := Image.NAXISn(1);
        if(Image.NAXISn'Last > 1)
        then
            RowLength := Image.NAXISn(2);
        else
            RowLength := 1;
        end if;

      New_Line;
      FITS_IO.Put_File_Type(In_File, "DBG> ");
    end;

    -- Set-up data read buffer

    declare

        --package Phys_Data is new FITS_IO.Data_Unit(Phys_Type);
       type Phys_Type_Arr is array (FITS_IO.Positive_Count range <>) of Phys_Type;
       procedure DU_Read is new FITS_IO.Read(Phys_Type, Phys_Type_Arr);

        -- example of data elaboration: find min max and count undef values

       -- subtype ColBuffer is Phys_Data.T_Arr(1..ColLength);
        subtype ColBuffer is Phys_Type_Arr(1..ColLength);

        procedure Analyze_Data
            (R : FITS_IO.Positive_Count;
            CurF32Col: ColBuffer;
            UValid : Boolean; UValue : Float)
        is
            V : Phys_Type;
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

        --Curr_Col : Phys_Data.T_Arr(1..ColLength);
        Curr_Col : Phys_Type_Arr(1..ColLength);
        Last : FITS_IO.Count;
     begin

         -- read all Data Unit

         for I in 1 .. RowLength
         loop
             --Phys_Data.Read(In_File, Curr_Col, Last);
             DU_Read(In_File, Curr_Col, Last);

--             TIO.New_Line;
--             TIO.New_Line;
--             for I in Curr_Col'Range loop TIO.Put(" "&Float'Image(Curr_Col(I))); end loop;
--            New_Line;
--            FITS_IO.Put_File_Type(In_File, "DBG> ");
 
             Analyze_Data(I,Curr_Col, Is_BLANK_In_Header, Undef_Phys);
         end loop;

        -- print results

         New_Line;
         if(Is_BLANK_In_Header)
         then
             TIO.Put_Line("Undef_Raw (BLANK)      : " & BLANK_Value);
             TIO.Put_Line("Undef_Phys             : " & Float'Image(Undef_Phys));
         end if;
         TIO.Put_Line("Undef_Count (NaN)      : " & FITS_IO.Count'Image(Undef_Count));
         TIO.Put_Line("Min                    : " & Phys_Type'Image(Min));
         TIO.Put_Line("Max                    : " & Phys_Type'Image(Max));

     end;

    Close(In_File);


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

