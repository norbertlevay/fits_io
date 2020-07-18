
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


    generic
    type T is private;
    type T_Arr is array (SIO.Positive_Count range <>) of T;
    type Tc is digits <>;
    with function "+"(R : Float_64) return Tc is <>;
    with function "+"(R : Float_32) return Tc is <>; 
    with function "+"(R : Integer_64) return Tc is <>; 
    with function "+"(R : Integer_32) return Tc is <>; 
    with function "+"(R : Integer_16) return Tc is <>; 
    with function "+"(R : Unsigned_8) return Tc is <>; 
    with function "+"(R : Tc) return T is <>;
    with function T_First return T is <>;
    with function T_Last  return T is <>;
    with function T_Image(V: T) return String is <>;
    with function T_Valid(V: T) return Boolean is <>;
    with function ">"(L,R : T)  return Boolean is <>;
    with function "<"(L,R : T)  return Boolean is <>;
    with function "+"(R : T) return Float_64 is <>;
    with function "+"(R : Float_64) return T is <>;
    with function To_V3Type(S : String) return T is <>;
    package T_App is

        use type SIO.Count;
        Special_Count : SIO.Count := 0; -- Inf...
        Undef_Count   : SIO.Count := 0; -- NaN
 
        procedure Plane_Data(A : T_Arr; C : SIO.Positive_Count);

        package   T_V3Image_Read is new V3_Image_Read(T,T_Arr,Tc);
        procedure Read_Data_Unit is new T_V3Image_Read.Read_Data_Unit_By_Planes(Plane_Data);

        procedure Put_Results;

    end T_App;

    package body T_App is

        Min : T := T_Last;
        Max : T := T_First;

        procedure Plane_Data(A : T_Arr; C : SIO.Positive_Count)
        is
            E : T;
        begin
            for I in A'Range
            loop
                E := A(I);
                
                if(not T_Valid(E))
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

        procedure Put_Results
        is
        begin
        TIO.Put_Line("Special_Count (Inf...) : " & SIO.Count'Image(Special_Count));
        TIO.Put_Line("Undef_Count (NaN)      : " & SIO.Count'Image(Undef_Count));
        TIO.Put_Line("Min                    : " & T_Image(Min));
        TIO.Put_Line("Max                    : " & T_Image(Max));
        end Put_Results;

    end T_App;

    package F64 is new T_App(Float_64,   F64_Arr, Float_64);
    package F32 is new T_App(Float_32,   F32_Arr, Float_32);
    package I64 is new T_App(Integer_64, I64_Arr, Float_64);
    package I32 is new T_App(Integer_32, I32_Arr, Float_64);
    package I16 is new T_App(Integer_16, I16_Arr, Float_32);
    package U8  is new T_App(Unsigned_8, U8_Arr,  Float_32);

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

            case(HDUInfo.BITPIX) is
                when -64 =>
                    F64.Read_Data_Unit(InFile,HDUInfo.BITPIX,HDUInfo.NAXISn'Last,HDUInfo.NAXISn, Cards);
                    F64.Put_Results;
                when -32 =>
                    F32.Read_Data_Unit(InFile,HDUInfo.BITPIX,HDUInfo.NAXISn'Last,HDUInfo.NAXISn, Cards);
                    F32.Put_Results;
                 when  64 =>
                    I64.Read_Data_Unit(InFile,HDUInfo.BITPIX,HDUInfo.NAXISn'Last,HDUInfo.NAXISn, Cards);
                    I64.Put_Results;
                 when  32 =>
                    I32.Read_Data_Unit(InFile,HDUInfo.BITPIX,HDUInfo.NAXISn'Last,HDUInfo.NAXISn, Cards);
                    I32.Put_Results;
                 when  16 =>
                    I16.Read_Data_Unit(InFile,HDUInfo.BITPIX,HDUInfo.NAXISn'Last,HDUInfo.NAXISn, Cards);
                    I16.Put_Results;
                 when   8 =>
                    U8.Read_Data_Unit(InFile,HDUInfo.BITPIX,HDUInfo.NAXISn'Last,HDUInfo.NAXISn, Cards);
                    U8.Put_Results;
                when others => null; -- FIXME Error
            end case;

        end;

    end;

    SIO.Close(InFile);

end minmax_V3_alt_4;

