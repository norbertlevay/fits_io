
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

package body V3_Privs_Image is

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

  -- BEGIN Physical_Private

  I64_BLANKin : Integer_64;-- FIXME convert from String
  I32_BLANKin : Integer_32;-- FIXME convert from String
  I16_BLANKin : Integer_16;-- FIXME convert from String
  U8_BLANKin  : Unsigned_8;-- FIXME convert from String

  -- helper funcs

  function TmF64_Is_Undef(Vin : in Float_64) return Boolean is begin return (not (Vin = Vin)); end TmF64_Is_Undef;
  function TmF32_Is_Undef(Vin : in Float_32) return Boolean is begin return (not (Vin = Vin)); end TmF32_Is_Undef;
  function TmI64_Is_Undef(Vin : in Integer_64) return Boolean is begin return (Vin = I64_BLANKin); end TmI64_Is_Undef;
  function TmI32_Is_Undef(Vin : in Integer_32) return Boolean is begin return (Vin = I32_BLANKin); end TmI32_Is_Undef;
  function TmI16_Is_Undef(Vin : in Integer_16) return Boolean is begin return (Vin = I16_BLANKin); end TmI16_Is_Undef;
  function TmU8_Is_Undef (Vin : in Unsigned_8) return Boolean is begin return (Vin = U8_BLANKin); end TmU8_Is_Undef;


  function TmF64_Handle_Undef(Vin : in Float_64) return Tm is begin return TmUndef; end TmF64_Handle_Undef;
  function TmF32_Handle_Undef(Vin : in Float_32) return Tm is begin return TmUndef; end TmF32_Handle_Undef;
  function TmI64_Handle_Undef(Vin : in Integer_64) return Tm is begin return TmUndef; end TmI64_Handle_Undef;
  function TmI32_Handle_Undef(Vin : in Integer_32) return Tm is begin return TmUndef; end TmI32_Handle_Undef;
  function TmI16_Handle_Undef(Vin : in Integer_16) return Tm is begin return TmUndef; end TmI16_Handle_Undef;
  function TmU8_Handle_Undef (Vin : in Unsigned_8) return Tm is begin return TmUndef; end TmU8_Handle_Undef;

  function "+"(R : Float_64) return Tcalc is begin return Tcalc(R); end "+";
  function "+"(R : Float_32) return Tcalc is begin return Tcalc(R); end "+";
  function "+"(R : Integer_64) return Tcalc is begin return Tcalc(R); end "+";
  function "+"(R : Integer_32) return Tcalc is begin return Tcalc(R); end "+";
  function "+"(R : Integer_16) return Tcalc is begin return Tcalc(R); end "+";
  function "+"(R : Unsigned_8) return Tcalc is begin return Tcalc(R); end "+";


  -- functionality &packages

  function TmF64_Linear   is new Linear_Private.Linear(Tcalc, TcBZERO, TcBSCALE, Float_64,   Tm, TmF64_Is_Undef, TmF64_Handle_Undef,"+","+");
  function TmF32_Linear   is new Linear_Private.Linear(Tcalc, TcBZERO, TcBSCALE, Float_32,   Tm, TmF32_Is_Undef, TmF32_Handle_Undef,"+","+");
  function TmI64_Linear   is new Linear_Private.Linear(Tcalc, TcBZERO, TcBSCALE, Integer_64, Tm, TmI64_Is_Undef, TmI64_Handle_Undef,"+","+");
  function TmI32_Linear   is new Linear_Private.Linear(Tcalc, TcBZERO, TcBSCALE, Integer_32, Tm, TmI32_Is_Undef, TmI32_Handle_Undef,"+","+");
  function TmI16_Linear   is new Linear_Private.Linear(Tcalc, TcBZERO, TcBSCALE, Integer_16, Tm, TmI16_Is_Undef, TmI16_Handle_Undef,"+","+");
  function TmU8_Linear    is new Linear_Private.Linear(Tcalc, TcBZERO, TcBSCALE, Unsigned_8, Tm, TmU8_Is_Undef,  TmU8_Handle_Undef, "+","+");

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
  -- END Physical_Private

--  package TmF64 is new Image_Data.FF(Tm, Tm_Arr, Tcalc, Float_64, TcBZERO, TcBSCALE, TmNaN,F64NaN);
--  package TmF32 is new Image_Data.FF(Tm, Tm_Arr, Tcalc, Float_32, TcBZERO, TcBSCALE, TmNaN,F32NaN);
--  package TmI64 is new Image_Data.FI(Tm, Tm_Arr, Tcalc, Integer_64, TcBZERO, TcBSCALE);
--  package TmI32 is new Image_Data.FI(Tm, Tm_Arr, Tcalc, Integer_32, TcBZERO, TcBSCALE);
--  package TmI16 is new Image_Data.FI(Tm, Tm_Arr, Tcalc, Integer_16, TcBZERO, TcBSCALE);
--  package TmU8  is new Image_Data.FU(Tm, Tm_Arr, Tcalc, Unsigned_8, TcBZERO, TcBSCALE);

begin
  TIO.Put_Line("DBG: V3_Privs_Image::Read_Volume");

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
      when   8 => TmU8.Read_Volume(F,DUStart,NAXISn, First,Last, Volume);
      when  16 => TmI16.Read_Volume(F,DUStart,NAXISn, First,Last, Volume);
      when  32 => TmI32.Read_Volume(F,DUStart,NAXISn, First,Last, Volume);
      when  64 => TmI64.Read_Volume(F,DUStart,NAXISn, First,Last, Volume);
      when -32 => TmF32.Read_Volume(F,DUStart,NAXISn, First,Last, Volume);
      when -64 => TmF64.Read_Volume(F,DUStart,NAXISn, First,Last, Volume);
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






end V3_Privs_Image;

