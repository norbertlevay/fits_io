
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Streams.Stream_IO;
with Ada.Command_Line; use Ada.Command_Line;

with V3_Types;  use V3_Types;
with V3_Arrays; use V3_Arrays;
with Pool_V3Type_Convs; use Pool_V3Type_Convs;
with Pool_String_To_V3Types; use Pool_String_To_V3Types;

with File;
with Physical_Read;
with Value;

with Optional;
with Optional.Reserved;
with Header;

with Pool_String_To_V3Types; use Pool_String_To_V3Types;
with V3_Pool_Scaling;         use V3_Pool_Scaling;

with T_Ops; use T_Ops;

procedure minmax is

    package TIO renames Ada.Text_IO;
    package SIO renames Ada.Streams.Stream_IO;


    generic
    type Tm is private;
    type Tm_Arr is array (SIO.Positive_Count range <>) of Tm;
    type Tc is digits <>;
    type Tf is private;

    with function To_V3Type(Arg : String) return Tf is <>;

    with function Init_UOut(UInValid : in Boolean; UIn : in Tf; UOutValid : in out Boolean; UOut : in out Tm) return Boolean is <>;

    with function Is_Undef(V,U : Tf; UValid : Boolean) return Boolean is <>;
    with function Is_Undef(V,U : Tm; UValid : Boolean) return Boolean is <>;


    with function "+"(R : Tf) return Tc is <>;
    with function "+"(R : Tc) return Tm is <>;

    with function T_First return Tm is <>;
    with function T_Last  return Tm is <>;
    with function T_Image(V: Tm) return String is <>;
    with function T_Valid(V: Tm) return Boolean is <>;
    with function ">"(L,R : Tm)  return Boolean is <>;
    with function "<"(L,R : Tm)  return Boolean is <>;
    with function To_V3Type(S : String) return Tm is <>;
    package T_App is

        use type SIO.Count;
        Special_Count : SIO.Count := 0; -- Inf...
        Undef_Count   : SIO.Count := 0; -- NaN

        procedure Plane_Data(E : Tm);
        procedure Undef_Data(E : Tm);

        package  T_Physical_Read is new Physical_Read(Tm,Tm_Arr,Tc, Tf);
        procedure Read_Data_Unit is new T_Physical_Read.Read_All(Plane_Data,Undef_Data);

        procedure Put_Results(UndefValid : in Boolean; UndefValue : in String);

    end T_App;

    package body T_App is


        Min : Tm := T_Last;
        Max : Tm := T_First;

        procedure Plane_Data(E : Tm)
        is
        begin
                if(not T_Valid(E))
                then
                    Special_Count := Special_Count + 1; -- Invalid but not NaN
                else
                    if(E > Max) then Max := E; end if;
                    if(E < Min) then Min := E; end if;
                end if;
        end Plane_Data;

        procedure Undef_Data(E : Tm)
        is
        begin
            Undef_Count   := Undef_Count   + 1;
        end Undef_Data;


        procedure Put_Results(UndefValid : in Boolean; UndefValue : in String)
        is
        begin
        TIO.Put_Line("UndefValid             : " & Boolean'Image(UndefValid));
        if(UndefValid) then TIO.Put_Line("UndefValue             : " & UndefValue); end if;
        TIO.Put_Line("Special_Count (Inf...) : " & SIO.Count'Image(Special_Count));
        TIO.Put_Line("Undef_Count (NaN)      : " & SIO.Count'Image(Undef_Count));
        TIO.Put_Line("Min                    : " & T_Image(Min));
        TIO.Put_Line("Max                    : " & T_Image(Max));
        end Put_Results;

    end T_App;


    package F64 is new T_App(Float_64,   F64_Arr, Float_64, Float_64);
    package F32 is new T_App(Float_32,   F32_Arr, Float_32, Float_32);
    package I64 is new T_App(Integer_64, I64_Arr, Float_64, Integer_64);
    package I32 is new T_App(Integer_32, I32_Arr, Float_64, Integer_32);
    package I16 is new T_App(Integer_16, I16_Arr, Float_32, Integer_16);
    package U8  is new T_App(Unsigned_8, U8_Arr,  Float_32, Unsigned_8);

    InFile   : SIO.File_Type;
    HDUStart : SIO.Positive_Count := 1; -- Primary HDU only

    UValid : Boolean := False;
    -- user does not wish to provide these vals (if he would,
    -- then those should be generic params and supplied separately at instantiation for each type)
    -- here we initialize only to avoid warning
    F64UValue : Float_64 := F64NaN;
    F32UValue : Float_32 := F32NaN;
    I64UValue : Integer_64 := Integer_64'Last;
    I32UValue : Integer_32 := Integer_32'Last;
    I16UValue : Integer_16 := Integer_16'Last;
    U8UValue : Unsigned_8 := Unsigned_8'Last;


    -- output NaN as hexadecimal
    function F64_To_U64 is
      new Ada.Unchecked_Conversion (Source => Float_64,
                                    Target => Unsigned_64);
    function F32_To_U32 is
      new Ada.Unchecked_Conversion (Source => Float_32,
                                    Target => Unsigned_32);

    package Hexa_F64 is new Ada.Text_IO.Modular_IO(Unsigned_64);
    package Hexa_F32 is new Ada.Text_IO.Modular_IO(Unsigned_32);
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
                    F64.Read_Data_Unit(InFile,HDUInfo.NAXISn, F64UValue, UValid, Cards);
                    F64.Put_Results(UValid, Float_64'Image(F64UValue));
                    TIO.Put("Hexa Undef : "); Hexa_F64.Put(F64_To_U64(F64UValue),  9,16);
                    TIO.New_Line;
                when -32 =>
                    F32.Read_Data_Unit(InFile,HDUInfo.NAXISn, F32UValue, UValid, Cards);
                    F32.Put_Results(UValid, Float_32'Image(F32UValue));
                    TIO.Put("Hexa Undef : "); Hexa_F32.Put(F32_To_U32(F32UValue),  9,16);
                    TIO.New_Line;
                 when  64 =>
                    I64.Read_Data_Unit(InFile,HDUInfo.NAXISn, I64UValue, UValid, Cards);
                    I64.Put_Results(UValid, Integer_64'Image(I64UValue));
                 when  32 =>
                    I32.Read_Data_Unit(InFile,HDUInfo.NAXISn, I32UValue, UValid, Cards);
                    I32.Put_Results(UValid, Integer_32'Image(I32UValue));
                 when  16 =>
                    I16.Read_Data_Unit(InFile,HDUInfo.NAXISn, I16UValue, UValid, Cards);
                    I16.Put_Results(UValid, Integer_16'Image(I16UValue));
                 when   8 =>
                    U8.Read_Data_Unit(InFile,HDUInfo.NAXISn, U8UValue, UValid, Cards);
                    U8.Put_Results(UValid, Unsigned_8'Image(U8UValue));
                when others => null; -- FIXME Error
            end case;

        end;

    end;

    SIO.Close(InFile);

end minmax;

