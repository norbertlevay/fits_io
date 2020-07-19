
with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Command_Line; use Ada.Command_Line;

with V3_Types;  use V3_Types;
with V3_Arrays; use V3_Arrays;
with Pool_V3Type_Convs; use Pool_V3Type_Convs;
with Pool_String_To_V3Types; use Pool_String_To_V3Types;

with File;
with V3_Image_Read;

with Optional;
with Optional.Reserved;
with Header;

with Pool_String_To_V3Types; use Pool_String_To_V3Types;
with V3_Pool_Linear;         use V3_Pool_Linear;


with T_Ops; use T_Ops;

procedure minmax_V3_alt_4 is

    package TIO renames Ada.Text_IO;
    package SIO renames Ada.Streams.Stream_IO;

    use type SIO.Count;
    Special_Count : SIO.Count := 0; -- Inf...
    Undef_Count   : SIO.Count := 0; -- NaN
 
    Min : Float_64 := Float_64'Last;
    Max : Float_64 := Float_64'First;

    procedure Plane_Data(A : F64_Arr; C : SIO.Positive_Count)
    is
        E : Float_64;
    begin
        for I in A'Range
        loop
            E := A(I);
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
        end loop;
    end Plane_Data;

    package   F64_V3Image_Read is new V3_Image_Read(Float_64,F64_Arr,Float_64);
    procedure Read_Data_Unit   is new F64_V3Image_Read.Read_Data_Unit_By_Planes(Plane_Data);

    InFile   : SIO.File_Type;
    HDUStart : SIO.Positive_Count := 1; -- Primary HDU only

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
            Cards   : Optional.Card_Arr := 
                Header.Read_Optional(InFile, Optional.Reserved.Reserved_Keys);
        begin
            Read_Data_Unit(InFile,HDUInfo.BITPIX,HDUInfo.NAXISn'Last,HDUInfo.NAXISn, Cards);
            TIO.Put_Line("Special_Count (Inf...) : " & SIO.Count'Image(Special_Count));
            TIO.Put_Line("Undef_Count (NaN)      : " & SIO.Count'Image(Undef_Count));
            TIO.Put_Line("Min                    : " & Float_64'Image(Min));
            TIO.Put_Line("Max                    : " & Float_64'Image(Max));
        end;
    end;

    SIO.Close(InFile);

end minmax_V3_alt_4;

