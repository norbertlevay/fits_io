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

with Image_Data;
--with Physical;


package body V3_Ints_Image is

package TIO renames Ada.Text_IO;


function Scan_Header
    (F : in SIO.File_Type;
    HDUStart : in  Positive_Count; -- blocks
    DUStart  : out Positive_Count; -- blocks
    BITPIX : out Integer;
    TcBZERO, TcBSCALE : in out Tcalc;
    BLANK_Val : out String) return NAXIS_Arr
is
begin

  File.Set_File_Block_Index(F, HDUStart);

  BLANK_Val :=(others => ' ');-- FIXME not nice; use Boolean to mark valid or not
    -- set 'null' meaning 'not found'

  declare
    Cards : Optional.Card_Arr := Read_Optional(F, Optional.Reserved.Reserved_Keys);
    Key : String(1..8);
  begin
    for I in Cards'Range
    loop
      Key := Cards(I)(1..8);
      if   (Key = "BZERO   ") then TcBZERO  := Tcalc'Value(Cards(I)(11..30));
      elsif(Key = "BSCALE  ") then TcBSCALE := Tcalc'Value(Cards(I)(11..30));
      elsif(Key = "BLANK   ") then BLANK_Val := Cards(I)(11..30);
      end if;
    end loop;
  end;

  File.Set_File_Block_Index(F, HDUStart);

  declare
    HDUInfo : File.HDU_Info_Type := File.Read_Header(F);
  begin
    DUStart := File.File_Block_Index(F);
    BITPIX  := HDUInfo.BITPIX;
    return HDUInfo.NAXISn;
  end;

end Scan_Header;







procedure Read_Volume
  (F : in SIO.File_Type;         -- File F
  HDUStart : in Positive_Count;  -- at offset FDUStart (referenced by HDUNum)
  First, Last : in NAXIS_Arr;    -- F and L point limiting the subcube of NAXISn
  Volume : out Tm_Arr)
is
  DUStart : Positive_Count;

  TcBZERO  : Tcalc := 0.0;
  TcBSCALE : Tcalc := 1.0;
  BLANK_Val : String(1 .. 20); -- card value

  package TmF64 is new Image_Data.rF(Tm, Tm_Arr, Tcalc, Float_64, TcBZERO, TcBSCALE, TmNaN,F64NaN);
  package TmF32 is new Image_Data.rF(Tm, Tm_Arr, Tcalc, Float_32, TcBZERO, TcBSCALE, TmNaN,F32NaN);
  package TmI64 is new Image_Data.II(Tm, Tm_Arr, Tcalc, Integer_64, TcBZERO, TcBSCALE);
  package TmI32 is new Image_Data.II(Tm, Tm_Arr, Tcalc, Integer_32, TcBZERO, TcBSCALE);
  package TmI16 is new Image_Data.II(Tm, Tm_Arr, Tcalc, Integer_16, TcBZERO, TcBSCALE);
  package TmU8  is new Image_Data.IU(Tm, Tm_Arr, Tcalc, Unsigned_8, TcBZERO, TcBSCALE);

begin
  TIO.Put_Line("DBG: V3_Ints_Image::Read_Volume");

  File.Set_File_Block_Index(F, HDUStart);

  declare
    BITPIX  : Integer;
    NAXISn  : NAXIS_Arr := Scan_Header(F, HDUStart,
                            DUStart,BITPIX, TcBZERO, TcBSCALE, BLANK_Val);
 begin

    TIO.Put_Line("HDUStart [blocks] : " & Positive_Count'Image(HDUStart));
    TIO.Put_Line("DUStart  [blocks] : " & Positive_Count'Image(DUStart));
    TIO.Put_Line("BITPIX : " & Integer'Image(BITPIX));
    TIO.Put_Line("BZERO  : " & Tcalc'Image(TcBZERO));
    TIO.Put_Line("BSCALE : " & Tcalc'Image(TcBSCALE));
    TIO.Put_Line("BLANK  : " & BLANK_Val);

    case(BITPIX) is
      when   8 => TmU8.Physical_In.Read_Volume(F,DUStart,NAXISn, First,Last, Volume);
      when  16 => TmI16.Physical_In.Read_Volume(F,DUStart,NAXISn, First,Last, Volume);
      when  32 => TmI32.Physical_In.Read_Volume(F,DUStart,NAXISn, First,Last, Volume);
      when  64 => TmI64.Physical_In.Read_Volume(F,DUStart,NAXISn, First,Last, Volume);
      when -32 => TmF32.Physical_In.Read_Volume(F,DUStart,NAXISn, First,Last, Volume);
      when -64 => TmF64.Physical_In.Read_Volume(F,DUStart,NAXISn, First,Last, Volume);
      when others => null; -- FIXME Error
    end case;

  end;

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
  NAXISi : in NAXIS_Arr) -- Tm_Arr has size NAXIS1 .. NAXISi, where i<=NAXISn'Length
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






end V3_Ints_Image;

