
with Ada.Text_IO; --use Ada.Text_IO;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed
with Mandatory; use Mandatory;-- NAXIS_Arr needed
with Optional;
with Optional.Reserved;
with Header; use Header;
with File;
with V3_Types; use V3_Types;-- types needed

--with Image_Data;
--with Physical;
with Physical_Private;
with Linear_Private;
with Scan_Header;


package body V3_Image_Data is

package TIO renames Ada.Text_IO;

procedure Read_Volume
  (F : in SIO.File_Type;         -- File F
  ImDataInfo  : in Scan_Header.Image_Data_Rec;  -- data from Header
  First, Last : in NAXIS_Arr;    -- F and L point limiting the subcube of NAXISn
  Volume : out Tm_Arr)
is
  DUStart : Positive_Count;

  TcBZERO  : Tcalc := 0.0;
  TcBSCALE : Tcalc := 1.0;
  BLANK_Val : String(1 .. 20); -- card value

  BLANKValid  : Boolean := False;
  I64_BLANKin : Integer_64;-- FIXME convert from String
  I32_BLANKin : Integer_32;-- FIXME convert from String
  I16_BLANKin : Integer_16;-- FIXME convert from String
  U8_BLANKin  : Unsigned_8;-- FIXME convert from String

  UndefOut : Tm := TmUndef; -- must be variable -> is i out generic param

  -- FIXME also missing case when BLANK is not supplied in Header -> Is_Undef() return False


  -- helper funcs

  function "+"(R : Float_64) return Tcalc is begin return Tcalc(R); end "+";
  function "+"(R : Float_32) return Tcalc is begin return Tcalc(R); end "+";
  function "+"(R : Integer_64) return Tcalc is begin return Tcalc(R); end "+";
  function "+"(R : Integer_32) return Tcalc is begin return Tcalc(R); end "+";
  function "+"(R : Integer_16) return Tcalc is begin return Tcalc(R); end "+";
  function "+"(R : Unsigned_8) return Tcalc is begin return Tcalc(R); end "+";

  -- Linear function

  function TmF64_Linear   is new Linear_Private.Linear_From_Floats(Tcalc, TcBZERO, TcBSCALE, Float_64,   Tm, BLANKValid, UndefOut, "+","+");
  function TmF32_Linear   is new Linear_Private.Linear_From_Floats(Tcalc, TcBZERO, TcBSCALE, Float_32,   Tm, BLANKValid, UndefOut, "+","+");
  function TmI64_Linear   is new Linear_Private.Linear_From_Ints(Tcalc, TcBZERO, TcBSCALE, Integer_64,   Tm, BLANKValid, I64_BLANKin, UndefOut, "+","+");
  function TmI32_Linear   is new Linear_Private.Linear_From_Ints(Tcalc, TcBZERO, TcBSCALE, Integer_32,   Tm, BLANKValid, I32_BLANKin, UndefOut, "+","+");
  function TmI16_Linear   is new Linear_Private.Linear_From_Ints(Tcalc, TcBZERO, TcBSCALE, Integer_16,   Tm, BLANKValid, I16_BLANKin, UndefOut, "+","+");
  function TmU8_Linear    is new Linear_Private.Linear_From_Ints(Tcalc, TcBZERO, TcBSCALE, Unsigned_8,   Tm, BLANKValid, U8_BLANKin,  UndefOut, "+","+");

  -- Packages

  package  TmF64_Physical is new Physical_Private(Tm, Tm_Arr, Tcalc, Float_64);
  package  TmF32_Physical is new Physical_Private(Tm, Tm_Arr, Tcalc, Float_32);
  package  TmI64_Physical is new Physical_Private(Tm, Tm_Arr, Tcalc, Integer_64);
  package  TmI32_Physical is new Physical_Private(Tm, Tm_Arr, Tcalc, Integer_32);
  package  TmI16_Physical is new Physical_Private(Tm, Tm_Arr, Tcalc, Integer_16);
  package  TmU8_Physical  is new Physical_Private(Tm, Tm_Arr, Tcalc, Unsigned_8);

  package  TmF64          is new TmF64_Physical.Input(TmF64_Linear);
  package  TmF32          is new TmF32_Physical.Input(TmF32_Linear);
  package  TmI64          is new TmI64_Physical.Input(TmI64_Linear);
  package  TmI32          is new TmI32_Physical.Input(TmI32_Linear);
  package  TmI16          is new TmI16_Physical.Input(TmI16_Linear);
  package  TmU8           is new TmU8_Physical.Input(TmU8_Linear);

begin
  TIO.Put_Line("DBG: V3_Image_Data::Read_Volume");
  --File.Set_File_Block_Index(F, DUStart);


    BLANKValid := ImDataInfo.BLANK_Valid;
    -- must be set: BLANKValid (Tnn_BLANKin) are global to Linear

  -- FIXME ?? make BLANK String_20 param to TmTf_Read_Volume and convert there ??
    case(ImDataInfo.BITPIX) is
      when   8 => 
          if(BLANKValid) then U8_BLANKin := Unsigned_8'Value(ImDataInfo.BLANK); end if;
          TmU8.Read_Volume (F,DUStart,ImDataInfo.NAXISn, First,Last, Volume);
      when  16 =>
          if(BLANKValid) then I16_BLANKin := Integer_16'Value(ImDataInfo.BLANK); end if;
          TmI16.Read_Volume(F,DUStart,ImDataInfo.NAXISn, First,Last, Volume);
      when  32 => 
          if(BLANKValid) then I32_BLANKin := Integer_32'Value(ImDataInfo.BLANK); end if;
          TmI32.Read_Volume(F,DUStart,ImDataInfo.NAXISn, First,Last, Volume);
      when  64 => 
          if(BLANKValid) then I64_BLANKin := Integer_64'Value(ImDataInfo.BLANK); end if;
          TmI64.Read_Volume(F,DUStart,ImDataInfo.NAXISn, First,Last, Volume);
      when -32 => TmF32.Read_Volume(F,DUStart,ImDataInfo.NAXISn, First,Last, Volume);
      when -64 => TmF64.Read_Volume(F,DUStart,ImDataInfo.NAXISn, First,Last, Volume);
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






end V3_Image_Data;

