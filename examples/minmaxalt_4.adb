
with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Command_Line; use Ada.Command_Line;

with V3_Types;  use V3_Types;
with V3_Arrays; use V3_Arrays;
with File;
with Physical_Read;

with Optional;
with Optional.Reserved;
with Header;

with Pool_String_To_V3Types; use Pool_String_To_V3Types;
with V3_Pool_Linear;         use V3_Pool_Linear;


procedure minmaxalt_4 is

    package TIO renames Ada.Text_IO;
    package SIO renames Ada.Streams.Stream_IO;

                                                         -- Tm          Tc          Tf    
    package F64_Physical_Read is new Physical_Read(Float_64, F64_Arr, Float_64, Float_64);
    package F32_Physical_Read is new Physical_Read(Float_64, F64_Arr, Float_64, Float_32);

    package I64_Physical_Read is new Physical_Read(Float_64, F64_Arr, Float_64, Integer_64);
    package I32_Physical_Read is new Physical_Read(Float_64, F64_Arr, Float_64, Integer_32);
    package I16_Physical_Read is new Physical_Read(Float_64, F64_Arr, Float_64, Integer_16);
    package U8_Physical_Read  is new Physical_Read(Float_64, F64_Arr, Float_64, Unsigned_8);


    use type SIO.Count;
    Special_Count : SIO.Count := 0; -- Inf...
    Undef_Count   : SIO.Count := 0; -- NaN
    Max : Float_64 := Float_64'First;
    Min : Float_64 := Float_64'Last;

    procedure F64_Data_Elem(E : Float_64)
    is
    begin
        if(not E'Valid)
        then
            if(E = E)
            then Special_Count := Special_Count + 1; -- Invalid but not NaN
            else Undef_Count   := Undef_Count   + 1; -- NaN
            end if;
        else
            if(E > Max) then Max := E; end if;
            if(E < Min) then Min := E; end if;
        end if;
    end F64_Data_Elem;

    procedure F64_Read_All is new F64_Physical_Read.Read_All(F64_Data_Elem);
    procedure F32_Read_All is new F32_Physical_Read.Read_All(F64_Data_Elem);
    procedure I64_Read_All is new I64_Physical_Read.Read_All(F64_Data_Elem);
    procedure I32_Read_All is new I32_Physical_Read.Read_All(F64_Data_Elem);
    procedure I16_Read_All is new I16_Physical_Read.Read_All(F64_Data_Elem);
    procedure U8_Read_All  is new U8_Physical_Read.Read_All (F64_Data_Elem);

    InFile   : SIO.File_Type;
    HDUStart : SIO.Positive_Count := 1; -- Primary HDU only$

begin

    if(Argument_Count /= 1 ) 
    then 
      TIO.Put_Line("Usage  " & Command_Name & " <file name>");
      return;
    else
      SIO.Open(InFile, SIO.In_File, (Argument(1)));
    end if;

    File.Set_File_Block_Index(InFile,HDUStart);

    declare
        HDUInfo : File.HDU_Info_Type := File.Read_Header(InFile);
    begin

        File.Set_File_Block_Index(InFile,HDUStart);

        declare
            Cards   : Optional.Card_Arr := Header.Read_Optional(InFile, Optional.Reserved.Reserved_Keys);
        begin

        case(HDUInfo.BITPIX) is
            when   8 => U8_Read_All (InFile,HDUInfo.NAXISn, Cards);
            when  16 => I16_Read_All(InFile,HDUInfo.NAXISn, Cards);
            when  32 => I32_Read_All(InFile,HDUInfo.NAXISn, Cards);
            when  64 => I64_Read_All(InFile,HDUInfo.NAXISn, Cards);
            when -32 => F32_Read_All(InFile,HDUInfo.NAXISn, Cards);
            when -64 => F64_Read_All(InFile,HDUInfo.NAXISn, Cards);
            when others => null; -- FIXME error
        end case;

        TIO.Put_Line("Special_Count (Inf...) : " & SIO.Count'Image(Special_Count));
        TIO.Put_Line("Undef_Count (NaN)      : " & SIO.Count'Image(Undef_Count));
        TIO.Put_Line("Min                    : " & Float_64'Image(Min));
        TIO.Put_Line("Max                    : " & Float_64'Image(Max));

        end;

    end;

    SIO.Close(InFile);

end minmaxalt_4;

