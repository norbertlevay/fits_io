-- NOTE from Read_Volume_As_Float:
--  if(BITPIX > 0)
--  then
    -- call ReadIntPlane()
--    null;
--  else
    -- call ReadFloatPlane()
--    null;
--  end if;

--DUSize  := Data_Unit_Size_elems(HDUInfo.NAXISn);

    -- Set Undef val in Read_Volume_As_Int/UInt()
--    case(BITPIX) is
--      when   8 => Undef_Value := ScaleUI8(BLANK_Str); -- UndefVal = A + B * BLANK_Str;
--      when  16 => Undef_Value := ScaleI16(BLANK_Str);
--      when -32 => Undef_Value := Tm(F64NaN);-- FIXME weird
--      when -64 => Undef_Value := Tm(F64NaN);-- FIXME weird
--      when others => null; -- FIXME Error
--    end case;

-- decode BLANK as string:
-- BLANK_Str : String(1..20);
-- ignore in Read_* _As_Float:      elsif(Key = "BLANK   ") then BLANK_Str := Cards(I)(11..30);


with Ada.Text_IO; --use Ada.Text_IO;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed
with Mandatory; use Mandatory;-- NAXIS_Arr needed
with Optional;
with Optional.Reserved;
with Header; use Header;
with File;
with V3_Types; use V3_Types;-- types needed

with FF;
with FI;
with Physical;

package body V3_Floats_Image is




procedure Read_Volume
  (F : in SIO.File_Type;         -- File F
  HDUStart : in Positive_Count;  -- at offset FDUStart (referenced by HDUNum)
  First, Last : in NAXIS_Arr;    -- F and L point limiting the subcube of NAXISn
  Undef_Value : out Tm;
  Volume : out Tm_Arr)
is
  DUStart : Positive_Count;

  TcBZERO  : Tcalc := 0.0;
  TcBSCALE : Tcalc := 1.0;

  package TmF64 is new FF(Tm, Tm_Arr, Tcalc, Float_64, TmNaN);
  package TmF32 is new FF(Tm, Tm_Arr, Tcalc, Float_32, TmNaN);
  package TmI16 is new FI(Tm, Tm_Arr, Tcalc, Integer_16);
begin
  Ada.Text_IO.Put_Line("DBG: V3_Floats_Image::Read_Volume");

  File.Set_File_Block_Index(F, HDUStart);

  declare
    Cards : Optional.Card_Arr := Read_Optional(F, Optional.Reserved.Reserved_Keys);
    Key : String(1..8);
  begin
    for I in Cards'Range
    loop
      Key := Cards(I)(1..8);
      if   (Key = "BZERO   ") then TcBZERO  := Tcalc'Value(Cards(I)(11..30));
      elsif(Key = "BSCALE  ") then TcBSCALE := Tcalc'Value(Cards(I)(11..30));
      end if;
    end loop;
  end;

  File.Set_File_Block_Index(F, HDUStart);

  declare
    HDUInfo : File.HDU_Info_Type := File.Read_Header(F);
    NAXISn  : NAXIS_Arr := HDUInfo.NAXISn;
    BITPIX  : Integer   := HDUInfo.BITPIX;
  begin
    DUStart := File.File_Block_Index(F);

    case(BITPIX) is
      when  16 =>
          TmI16.Phys.Read_Volume(F,DUStart,NAXISn, First,Last, TcBZERO,TcBSCALE, Volume);
      when -32 => 
          TmF32.Phys.Read_Volume(F,DUStart,NAXISn, First,Last, TcBZERO,TcBSCALE, Volume);
      when -64 =>
          TmF64.Phys.Read_Volume(F,DUStart,NAXISn, First,Last, TcBZERO,TcBSCALE, Volume);
      when others => null; -- FIXME Error
    end case;

  end;

end Read_Volume;







procedure Write_Volume
  (F : in SIO.File_Type;
  HDUStart : in Positive_Count;
  NAXISn : in NAXIS_Arr;
  First, Last : in NAXIS_Arr;
  Volume : in Tm_Arr)
is
begin
  null;
end Write_Volume;










-- FIXME how to give "none" no BLANK provided (for raw Integers data)
procedure Read_Data_Unit_By_Planes
  (F : in SIO.File_Type;
  NAXISi : in NAXIS_Arr; -- Tm_Arr has size NAXIS1 .. NAXISi, where i<=NAXISn'Length
  Undef_Value : out Tm) -- FIXME how to return "none"=no BLANK (for raw Integers data)
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
    NAXISi : in NAXIS_Arr; -- Tm_Arr has size NAXIS1 .. NAXISi, where i<=NAXISn'Length
    Undef_Value : in Tm) -- FIXME how to return "none"=no BLANK (for raw Integers data)
is
begin
  null;
end Write_Data_Unit_By_Planes;






end V3_Floats_Image;

