
with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Command_Line;
with Ada.Unchecked_Conversion;

with V3_Types;          use V3_Types;

with File;              -- HDU_Info_Type needed
with Optional;          -- Card_Arr needed
with Optional.Reserved; -- Reserved cards needed
with Header;            -- Header.Read_Optional needed

with Scaling;
with V3_Pool_Scaling;   use V3_Pool_Scaling;

with DU_Type;
with DU_Type.Physical;
--with DU_Type.Physical.Data_Unit;
with DU_Type.Minmax;

procedure minmax_all_types is

    package TIO renames Ada.Text_IO;
    package SIO renames Ada.Streams.Stream_IO;
    package CLI renames Ada.Command_Line;

    package PF64 is new DU_Type(Float_64,    Float_64, Float_64);
    package PF32 is new DU_Type(Float_32,    Float_32, Float_32);
    package PI64 is new DU_Type(Integer_64,  Float_64, Integer_64);
    package PI32 is new DU_Type(Integer_32,  Float_64, Integer_32);
    package PI16 is new DU_Type(Integer_16,  Float_32, Integer_16);
    package PU8  is new DU_Type(Unsigned_8,   Float_32, Unsigned_8);


    package F64R_Scaling is new Scaling(Float_64,   Float_64, Float_64);
    package F32R_Scaling is new Scaling(Float_32,   Float_32, Float_32);
    package I64R_Scaling is new Scaling(Integer_64, Float_64, Integer_64);
    package I32R_Scaling is new Scaling(Integer_32, Float_64, Integer_32);
    package I16R_Scaling is new Scaling(Integer_16, Float_32, Integer_16);
    package  U8R_Scaling is new Scaling(Unsigned_8, Float_32, Unsigned_8);

    package F64_Phys   is new PF64.Physical(F64R_Scaling,F64R_Scaling);
    package F32_Phys   is new PF32.Physical(F32R_Scaling,F32R_Scaling);
    package I64_Phys   is new PI64.Physical(I64R_Scaling,I64R_Scaling);
    package I32_Phys   is new PI32.Physical(I32R_Scaling,I32R_Scaling);
    package I16_Phys   is new PI16.Physical(I16R_Scaling,I16R_Scaling);
    package U8_Phys    is new PU8.Physical(U8R_Scaling,U8R_Scaling);

--    package F64_PhysDU is new F64_Phys.Data_Unit;
--    package F32_PhysDU is new F32_Phys.Data_Unit;
--    package I64_PhysDU is new I64_Phys.Data_Unit;
--    package I32_PhysDU is new I32_Phys.Data_Unit;
--    package I16_PhysDU is new I16_Phys.Data_Unit;
--    package U8_PhysDU  is new U8_Phys.Data_Unit;

    -- app code for all T
    package F64 is new PF64.Minmax;--(F64_Phys);--,F64_PhysDU);
    package F32 is new PF32.Minmax;--(F32_Phys);--,F32_PhysDU);
    package I64 is new PI64.Minmax;--(I64_Phys);--,I64_PhysDU);
    package I32 is new PI32.Minmax;--(I32_Phys);--,I32_PhysDU);
    package I16 is new PI16.Minmax;--(I16_Phys);--,I16_PhysDU);
    package U8  is new PU8.Minmax;--(U8_Phys);--,U8_PhysDU);

    procedure F64_Read_Data_Unit is new F64_Phys.Read_Data_Unit(F64.Plane_Data);
    procedure F32_Read_Data_Unit is new F32_Phys.Read_Data_Unit(F32.Plane_Data);
    procedure I64_Read_Data_Unit is new I64_Phys.Read_Data_Unit(I64.Plane_Data);
    procedure I32_Read_Data_Unit is new I32_Phys.Read_Data_Unit(I32.Plane_Data);
    procedure I16_Read_Data_Unit is new I16_Phys.Read_Data_Unit(I16.Plane_Data);
    procedure U8_Read_Data_Unit is new U8_Phys.Read_Data_Unit(U8.Plane_Data);




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

    if(CLI.Argument_Count /= 1 ) 
    then 
      TIO.Put_Line("Usage  " & CLI.Command_Name & " <file name>");
      return;
    else
      SIO.Open(InFile, SIO.In_File, (CLI.Argument(1)));
    end if;

    File.Set_File_Block_Index(InFile,HDUStart);

    declare
        HDUInfo : File.HDU_Info_Type := File.Read_Header(InFile);
    begin

        File.Set_File_Block_Index(InFile,HDUStart);

        declare
            Cards   : Optional.Card_Arr := 
            Header.Read_Optional(InFile, Optional.Reserved.Reserved_Keys);
            UInValid : Boolean := False;
        begin

            case(HDUInfo.BITPIX) is
                when -64 =>
                    declare
                        UInValue : Float_64;
                        A,B : Float_64;
                    begin
                        F64_Phys.Header_Info(Cards, A,B, UInValid, UInValue);
                        PF64.Init_Undef_For_Read(UInValid, UInValue, UValid, F64UValue);
                        F64_Read_Data_Unit(InFile,HDUInfo.NAXISn, A,B);
                    end;
                    F64.Put_Results(UValid, Float_64'Image(F64UValue));
                    TIO.Put("Hexa Undef : "); Hexa_F64.Put(F64_To_U64(F64UValue),  9,16);
                    TIO.New_Line;

                when -32 =>
                     declare
                        UInValue : Float_32;
                        A,B : Float_32;
                    begin
                        F32_Phys.Header_Info(Cards, A,B, UInValid, UInValue);
                        PF32.Init_Undef_For_Read(UInValid, UInValue, UValid, F32UValue);
                        F32_Read_Data_Unit(InFile,HDUInfo.NAXISn, A,B);
                    end;
                    F32.Put_Results(UValid, Float_32'Image(F32UValue));
                    TIO.Put("Hexa Undef : "); Hexa_F32.Put(F32_To_U32(F32UValue),  9,16);
                    TIO.New_Line;

                 when  64 =>
                    declare
                        UInValue : Integer_64;
                        A,B : Float_64;
                    begin
                        I64_Phys.Header_Info(Cards, A,B, UInValid, UInValue);
                        PI64.Init_Undef_For_Read(UInValid, UInValue, UValid, I64UValue);
                        I64_Read_Data_Unit(InFile,HDUInfo.NAXISn, A,B);
                    end;
                    I64.Put_Results(UValid, Integer_64'Image(I64UValue));
                 when  32 =>
                    declare
                        UInValue : Integer_32;
                        A,B : Float_64;
                    begin
                        I32_Phys.Header_Info(Cards, A,B, UInValid, UInValue);
                        PI32.Init_Undef_For_Read(UInValid, UInValue, UValid, I32UValue);
                        I32_Read_Data_Unit(InFile,HDUInfo.NAXISn, A,B);
                    end;
                    I32.Put_Results(UValid, Integer_32'Image(I32UValue));

                 when  16 =>
                    declare
                        UInValue : Integer_16;
                        A,B : Float_32;
                    begin
                        I16_Phys.Header_Info(Cards, A,B, UInValid, UInValue);
                        PI16.Init_Undef_For_Read(UInValid, UInValue, UValid, I16UValue);
                        I16_Read_Data_Unit(InFile,HDUInfo.NAXISn, A,B);
                    end;
                    I16.Put_Results(UValid, Integer_16'Image(I16UValue));
                 when   8 =>
                     declare
                        UInValue : Unsigned_8;
                        A,B : Float_32;
                    begin
                        U8_Phys.Header_Info(Cards, A,B, UInValid, UInValue);
                        PU8.Init_Undef_For_Read(UInValid, UInValue, UValid, U8UValue);
                        U8_Read_Data_Unit(InFile,HDUInfo.NAXISn, A,B);
                    end;
                    U8.Put_Results(UValid, Unsigned_8'Image(U8UValue));

                when others => null; -- FIXME Error
            end case;

        end;

    end;

    SIO.Close(InFile);

end minmax_all_types;

