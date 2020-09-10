
with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Command_Line;
with Ada.Unchecked_Conversion;

with V3_Types;          use V3_Types;

with File;              -- HDU_Info_Type needed
with Optional;          -- Card_Arr needed
with Optional.Reserved; -- Reserved cards needed
with Header;            -- Header.Read_Optional needed

-- we instantiate here TT_App which need
-- these type-dependent implementations:
with V3_Pool_Scaling;   use V3_Pool_Scaling;

with TT_App;
with TT_App.Minmax;

procedure minmax is

    package TIO renames Ada.Text_IO;
    package SIO renames Ada.Streams.Stream_IO;
    package CLI renames Ada.Command_Line;


    package TF64 is new TT_App(Float_64,   F64_Arr, Float_64, Float_64);
    package TF32 is new TT_App(Float_32,   F32_Arr, Float_32, Float_32);
    package TI64 is new TT_App(Integer_64, I64_Arr, Float_64, Integer_64);
    package TI32 is new TT_App(Integer_32, I32_Arr, Float_64, Integer_32);
    package TI16 is new TT_App(Integer_16, I16_Arr, Float_32, Integer_16);
    package TU8  is new TT_App(Unsigned_8, U8_Arr,  Float_32, Unsigned_8);

    package F64 is new TF64.Minmax;
    package F32 is new TF32.Minmax;
    package I64 is new TI64.Minmax;
    package I32 is new TI32.Minmax;
    package I16 is new TI16.Minmax;
    package U8  is new TU8.Minmax;


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
                        F64.T_Physical.Header_Info(Cards, A,B, UInValid, UInValue);
                        F64.T_Physical.Init_Undef_For_Read(UInValid, UInValue, UValid, F64UValue);
                        F64.Read_Data_Unit(InFile,HDUInfo.NAXISn, A,B);
                    end;
                    F64.Put_Results(UValid, Float_64'Image(F64UValue));
                    TIO.Put("Hexa Undef : "); Hexa_F64.Put(F64_To_U64(F64UValue),  9,16);
                    TIO.New_Line;

                when -32 =>
                     declare
                        UInValue : Float_32;
                        A,B : Float_32;
                    begin
                        F32.T_Physical.Header_Info(Cards, A,B, UInValid, UInValue);
                        F32.T_Physical.Init_Undef_For_Read(UInValid, UInValue, UValid, F32UValue);
                        F32.Read_Data_Unit(InFile,HDUInfo.NAXISn, A,B);
                    end;
                    F32.Put_Results(UValid, Float_32'Image(F32UValue));
                    TIO.Put("Hexa Undef : "); Hexa_F32.Put(F32_To_U32(F32UValue),  9,16);
                    TIO.New_Line;

                 when  64 =>
                    declare
                        UInValue : Integer_64;
                        A,B : Float_64;
                    begin
                        I64.T_Physical.Header_Info(Cards, A,B, UInValid, UInValue);
                        I64.T_Physical.Init_Undef_For_Read(UInValid, UInValue, UValid, I64UValue);
                        I64.Read_Data_Unit(InFile,HDUInfo.NAXISn, A,B);
                    end;
                    I64.Put_Results(UValid, Integer_64'Image(I64UValue));
                 when  32 =>
                    declare
                        UInValue : Integer_32;
                        A,B : Float_64;
                    begin
                        I32.T_Physical.Header_Info(Cards, A,B, UInValid, UInValue);
                        I32.T_Physical.Init_Undef_For_Read(UInValid, UInValue, UValid, I32UValue);
                        I32.Read_Data_Unit(InFile,HDUInfo.NAXISn, A,B);
                    end;
                    I32.Put_Results(UValid, Integer_32'Image(I32UValue));

                 when  16 =>
                    declare
                        UInValue : Integer_16;
                        A,B : Float_32;
                    begin
                        I16.T_Physical.Header_Info(Cards, A,B, UInValid, UInValue);
                        I16.T_Physical.Init_Undef_For_Read(UInValid, UInValue, UValid, I16UValue);
                        I16.Read_Data_Unit(InFile,HDUInfo.NAXISn, A,B);
                    end;
                    I16.Put_Results(UValid, Integer_16'Image(I16UValue));
                 when   8 =>
                     declare
                        UInValue : Unsigned_8;
                        A,B : Float_32;
                    begin
                        U8.T_Physical.Header_Info(Cards, A,B, UInValid, UInValue);
                        U8.T_Physical.Init_Undef_For_Read(UInValid, UInValue, UValid, U8UValue);
                        U8.Read_Data_Unit(InFile,HDUInfo.NAXISn, A,B);
                    end;
                    U8.Put_Results(UValid, Unsigned_8'Image(U8UValue));

                when others => null; -- FIXME Error
            end case;

        end;

    end;

    SIO.Close(InFile);

end minmax;

