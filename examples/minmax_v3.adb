
with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Command_Line; use Ada.Command_Line;

with V3_Types;  use V3_Types;

with File;
with DU_Type.V3_Image_Read;

with Optional;
with Optional.Reserved;
with Header;

with V3_Pool_Scaling;  use V3_Pool_Scaling;


procedure minmax_V3 is

    package TIO renames Ada.Text_IO;
    package SIO renames Ada.Streams.Stream_IO;

    use type SIO.Count;
    Special_Count : SIO.Count := 0; -- Inf...
    Undef_Count   : SIO.Count := 0; -- NaN

    subtype Tcc is Float_64;-- do scaling in this type (Float only)
    subtype Tmm is Float_32;-- work with data in this type
    type Tmm_Arr is array (SIO.Positive_Count range <>) of Tmm;

    Min : Tmm := Tmm'Last;
    Max : Tmm := Tmm'First;

    -- user does not provide Undef value
    UValue : Tmm     := Tmm'Last;
    UValid : Boolean := False;
 
    procedure Plane_Data(A : Tmm_Arr; C : SIO.Positive_Count)
    is
        E : Tmm;
    begin
        for I in A'Range
        loop
            E := A(I);
            --TIO.Put(" " & Float_32'Image(E));
            if(not E'Valid) -- FIXME only for Tmm = Floats !
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

    package   F64 is new DU_Type(Tmm,Tmm_Arr,Tcc);
    package   F64_V3Image_Read is new F64.V3_Image_Read;
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

            Read_Data_Unit(InFile, HDUInfo.BITPIX, HDUInfo.NAXISn'Last - 1, HDUInfo.NAXISn, UValue, UValid, Cards);
            --Read_Data_Unit(InFile, HDUInfo.BITPIX, HDUInfo.NAXISn'Last, HDUInfo.NAXISn, UValue, UValid, Cards);

            TIO.Put_Line("Undef_Valid            : " & Boolean'Image(UValid));
            if(UValid)
            then
                TIO.Put_Line("Undef_Value            : " & Tmm'Image(UValue));
            end if;
            TIO.Put_Line("Special_Count (Inf...) : " & SIO.Count'Image(Special_Count));
            TIO.Put_Line("Undef_Count (NaN)      : " & SIO.Count'Image(Undef_Count));
            TIO.Put_Line("Min                    : " & Tmm'Image(Min));
            TIO.Put_Line("Max                    : " & Tmm'Image(Max));
        end;
    end;

    SIO.Close(InFile);

end minmax_V3;

