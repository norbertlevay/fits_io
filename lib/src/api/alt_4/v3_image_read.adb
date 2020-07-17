
with Ada.Text_IO; --use Ada.Text_IO;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed
with Mandatory; use Mandatory;-- NAXIS_Arr needed
with Optional;
with Optional.Reserved;
with Header; use Header;
with File;
with V3_Types; use V3_Types;-- types needed

with Physical_Read;
with Linear_Impl; use Linear_Impl;-- needed From_Header() for Header_Info instance
with Pool_String_To_V3Types; use Pool_String_To_V3Types; -- needed by Header_Info instance
with Pool_V3Type_Convs; use  Pool_V3Type_Convs;

package body V3_Image_Read is

package TIO renames Ada.Text_IO;

function Linear is new Linear_Pure(Float_64, Tcalc, Tm);
function Linear is new Linear_Pure(Float_32, Tcalc, Tm);
function Linear is new Linear_Pure(Integer_64, Tcalc, Tm);
function Linear is new Linear_Pure(Integer_32, Tcalc, Tm);
function Linear is new Linear_Pure(Integer_16, Tcalc, Tm);
function Linear is new Linear_Pure(Unsigned_8, Tcalc, Tm);



procedure Read_Volume
  (F : in SIO.File_Type;
  DUStart : in Positive_Count;
  BITPIX : Integer;
  NAXISn : NAXIS_Arr;
  First, Last : in NAXIS_Arr;
  Volume : out Tm_Arr;
  Cards : in Optional.Card_Arr)
is

    package F64_Physical_Read is new Physical_Read(Tm, Tm_Arr, Tcalc, Float_64);
    package F32_Physical_Read is new Physical_Read(Tm, Tm_Arr, Tcalc, Float_32);
    package I64_Physical_Read is new Physical_Read(Tm, Tm_Arr, Tcalc, Integer_64);
    package I32_Physical_Read is new Physical_Read(Tm, Tm_Arr, Tcalc, Integer_32);
    package I16_Physical_Read is new Physical_Read(Tm, Tm_Arr, Tcalc, Integer_16);
    package U8_Physical_Read  is new Physical_Read(Tm, Tm_Arr ,Tcalc, Unsigned_8);

begin
  TIO.Put_Line("DBG: V3_Image_Read::Read_Volume");

  -- ? File.Set_File_Block_Index(F, DUStart);

    case(BITPIX) is
      when   8 => U8_Physical_Read.Read_Volume (F,DUStart,NAXISn, First,Last, Volume, Cards);
      when  16 => I16_Physical_Read.Read_Volume(F,DUStart,NAXISn, First,Last, Volume, Cards);
      when  32 => I32_Physical_Read.Read_Volume(F,DUStart,NAXISn, First,Last, Volume, Cards);
      when  64 => I64_Physical_Read.Read_Volume(F,DUStart,NAXISn, First,Last, Volume, Cards);
      when -32 => F32_Physical_Read.Read_Volume(F,DUStart,NAXISn, First,Last, Volume, Cards);
      when -64 => F64_Physical_Read.Read_Volume(F,DUStart,NAXISn, First,Last, Volume, Cards);
      when others => null; -- FIXME Error
    end case;

end Read_Volume;







procedure Write_Volume
  (F : in SIO.File_Type;
  HDUStart : in Positive_Count;
  First, Last : in NAXIS_Arr;
  NAXISn : in NAXIS_Arr;
  Volume : in Tm_Arr)
is
begin
  null;
end Write_Volume;










-- FIXME how to give "none" no BLANK provided (for raw Integers data)
procedure Read_Data_Unit_By_Planes
  (F : in SIO.File_Type;
  NAXISi : in NAXIS_Arr; -- Tm_Arr has size NAXIS1 .. NAXISi, where i<=NAXISn'Length
  Cards : in Optional.Card_Arr)
is
  BITPIX : Integer;
  NAXISn : NAXIS_Arr(1..1);-- FIXME
  Length : Positive_Count := 1; -- calc from NAXISn
  Plane : Tm_Arr(1 .. Length);
  F64BZERO, F64BSCALE : Tcalc;
begin
  -- read Header keys
  -- FIXME read BZERO BSCALE (BLANK)

  -- FIXME revert: put loop inside case() to avoid check BITPIX at each turn
  for I in 1 .. (1 + NAXISn'Length - Length)
  loop
    case(BITPIX) is
--      when   8 =>  U8_Physical.Read_Array(F, F64BZERO, F64BSCALE, Plane);
--      when  16 => I16_Physical.Read_Array(F, F64BZERO, F64BSCALE, Plane);
--      when -32 => F32_Physical.Read_Array(F, F64BZERO, F64BSCALE, Undef_Value, Plane);
--      when -64 => F64_Physical.Read_Array(F, F64BZERO, F64BSCALE, Undef_Value, Plane);
      when others => null; -- FIXME Error
    end case;
    Plane_Data(Plane, I);
  end loop;

end Read_Data_Unit_By_Planes;





procedure Write_Data_Unit_By_Planes
    (F : in SIO.File_Type;
    NAXISi : in NAXIS_Arr) -- Tm_Arr has size NAXIS1 .. NAXISi, where i<=NAXISn'Length
is
begin
  null;
end Write_Data_Unit_By_Planes;






end V3_Image_Read;

