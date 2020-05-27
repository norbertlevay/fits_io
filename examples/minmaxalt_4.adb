
with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Command_Line; use Ada.Command_Line;

with V3_Types; use V3_Types;
with V3_Arrays; use V3_Arrays;
with File;
with Physical_Read;

with Optional;
with Optional.Reserved;
with Header;


with Pool_String_To_V3Types; use Pool_String_To_V3Types;
with V3_Pool_HInfo_Linear; use V3_Pool_HInfo_Linear;

procedure minmaxalt_4 is

    package TIO renames Ada.Text_IO;
    package SIO renames Ada.Streams.Stream_IO;


    package F64_Physical_Read is new Physical_Read(Float_64, F64_Arr, Float_64);
    package F32_Physical_Read is new Physical_Read(Float_64, F64_Arr, Float_32);

    package I64_Physical_Read is new Physical_Read(Float_64, F64_Arr, Integer_64);
    package I32_Physical_Read is new Physical_Read(Float_64, F64_Arr, Integer_32);
    package I16_Physical_Read is new Physical_Read(Float_64, F64_Arr, Integer_16);
    package U8_Physical_Read  is new Physical_Read(Float_64, F64_Arr ,Unsigned_8);


    -- BEGIN application : count undefined values
    use type SIO.Count;
    Undef_Count : SIO.Count := 0;

    procedure F64_Data_Elem(E : Float_64) is begin if(not E'Valid) then Undef_Count := Undef_Count + 1; end if; end F64_Data_Elem;
--    procedure F32_Data_Elem(E : Float_32) is begin if(not E'Valid) then Undef_Count := Undef_Count + 1; end if; end F32_Data_Elem;
    -- FIXME 'Valid will filter also Inf besides NaN

--    I64_UndefVal : Integer_64;
--    I32_UndefVal : Integer_32;
--    I16_UndefVal : Integer_16;
--    U8_UndefVal  : Unsigned_8;

--    procedure I64_Data_Elem(E : Integer_64) is begin if(E = I64_UndefVal) then Undef_Count := Undef_Count + 1; end if; end I64_Data_Elem;
--    procedure I32_Data_Elem(E : Integer_32) is begin if(E = I32_UndefVal) then Undef_Count := Undef_Count + 1; end if; end I32_Data_Elem;
--    procedure I16_Data_Elem(E : Integer_16) is begin if(E = I16_UndefVal) then Undef_Count := Undef_Count + 1; end if; end I16_Data_Elem;
--    procedure U8_Data_Elem (E : Unsigned_8) is begin if(E = U8_UndefVal)  then Undef_Count := Undef_Count + 1; end if; end U8_Data_Elem;


    procedure F64_Read_All is new F64_Physical_Read.Read_All(F64_Data_Elem);
    procedure F32_Read_All is new F32_Physical_Read.Read_All(F64_Data_Elem);
    procedure I64_Read_All is new I64_Physical_Read.Read_All(F64_Data_Elem);
    procedure I32_Read_All is new I32_Physical_Read.Read_All(F64_Data_Elem);
    procedure I16_Read_All is new I16_Physical_Read.Read_All(F64_Data_Elem);
    procedure U8_Read_All  is new U8_Physical_Read.Read_All (F64_Data_Elem);
    -- FIXME use pool for this and generic for implement

    -- END application

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

        TIO.Put_Line("Undef_Count : " & SIO.Count'Image(Undef_Count));

        end;
    end; -- Scan_Header

    SIO.Close(InFile);

end minmaxalt_4;
