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

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed or is there FITS.Positive_count?
with Mandatory; use Mandatory;-- NAXIS_Arr needed
with Optional;
with Optional.Reserved;
with Header; use Header;
with File;
with V3_Types; use V3_Types;-- types needed
with Physical; use Physical;

with Data_Funcs; use Data_Funcs;-- Set_File_Block_Index needed

package body V3_FITS_Image is

procedure Read_Dimensions
  (F : in SIO.File_Type;
  BITPIX : out Integer;
  NAXISn : out NAXIS_Arr)
is
begin
  null;
end Read_Dimensions;


function Read_Undef_Value(F : in SIO.File_Type) return Tm
is
  Vm : Tm;
begin
  return Vm;
end Read_Undef_Value;


-- Read data


procedure Read_Array_As_Float
  (F : in SIO.File_Type;
  Undef_Value : out Tm;   -- FIXME how to give "none" no BLANK provided (for raw Integers data)
  I : in Positive;-- needed to derive Tm_Arr length = NAXIS1*NAXIS2*...*NAXISi
  Plane : out Tm_Arr)
is
  BITPIX : Integer;
  NAXISn : NAXIS_Arr(1..1);-- FIXME
  Length : Positive_Count := 1; -- calc from NAXISn

  -- NOTE Scaling always calced in Float_64 (=Tc)
  -- For starters: always Float_64: Card-Values string has 20 chars 
  -- -> Float_64 has 15 digits validity -> use for card-value floating point always Float 64
  subtype Tcalc is Float_64;
  F64BZERO, F64BSCALE : Tcalc;
-- From Tc -> Tm
  function "+"(R : in Tcalc) return Tm is begin return Tm(R); end "+";

  -- From Tf -> Tc
  function "+"(R : in Unsigned_8) return Tcalc is begin return Tcalc(R); end "+";
  function "+"(R : in Integer_16) return Tcalc is begin return Tcalc(R); end "+";
  function "+"(R : in Float_32)   return Tcalc is begin return Tcalc(R); end "+";
  function "+"(R : in Tcalc)   return Float_32 is begin return Float_32(R); end "+";

  --                                                     Tf                   Tc
  procedure  U8ReadIntPlane is      new Read_Int_Array(Unsigned_8,Tm,Tm_Arr,Tcalc,"+","+");
  procedure I16ReadIntPlane is      new Read_Int_Array(Integer_16,Tm,Tm_Arr,Tcalc,"+","+");
  procedure F32F64ReadFloatPlane is new Read_Float_Array(Float_32,Tm,Tm_Arr,Tcalc,"+","+");
  procedure F64F64ReadFloatPlane is new Read_Float_Array(Float_64,Tm,Tm_Arr,Tcalc,"+","+");

begin
  Read_Dimensions(F, BITPIX, NAXISn);

  -- FIXME read BZERO BSCALE (BLANK)

  case(BITPIX) is
    when   8 => U8ReadIntPlane   (F, F64BZERO, F64BSCALE, Plane);
    when  16 => I16ReadIntPlane  (F, F64BZERO, F64BSCALE, Plane);
    when -32 => F32F64ReadFloatPlane(F, F64BZERO, F64BSCALE, Undef_Value, Plane);
    when -64 => F64F64ReadFloatPlane(F, F64BZERO, F64BSCALE, Undef_Value, Plane);
    when others => null; -- FIXME Error
  end case;

end Read_Array_As_Float;








procedure Read_Volume_As_Float
  (F : in SIO.File_Type;         -- File F
  HDUStart : in Positive_Count;  -- at offset FDUStart (referenced by HDUNum)
  First, Last : in NAXIS_Arr;    -- F and L point limiting the subcube of NAXISn
  Undef_Value : out Tm;
  Volume : out Tm_Arr)
is
  DUStart : Positive_Count;

  subtype Tcalc is Float_64;
  F64BZERO  : Tcalc := 0.0;
  F64BSCALE : Tcalc := 1.0;

  -- From Tc -> Tm
  function "+"(R : in Tcalc) return Tm is begin return Tm(R); end "+";

  -- From Tf -> Tc
  function "+"(R : in Unsigned_8) return Tcalc is begin return Tcalc(R); end "+";
  function "+"(R : in Integer_16) return Tcalc is begin return Tcalc(R); end "+";
  function "+"(R : in Float_32)   return Tcalc is begin return Tcalc(R); end "+";
  function "+"(R : in Tcalc)   return Float_32 is begin return Float_32(R); end "+";

  procedure  U8ReadIntVolume      is new Read_Int_Volume(Unsigned_8,Tm,Tm_Arr,Tcalc,"+","+");
  procedure I16ReadIntVolume      is new Read_Int_Volume(Integer_16,Tm,Tm_Arr,Tcalc,"+","+");
  procedure F32F64ReadFloatVolume is new Read_Float_Volume(Float_32,Tm,Tm_Arr,Tcalc,"+","+");
  procedure F64F64ReadFloatVolume is new Read_Float_Volume(Float_64,Tm,Tm_Arr,Tcalc,"+","+");
begin
  Ada.Text_IO.Put_Line("DBG: Read_Volume_As_Float");


  Set_File_Block_Index(F, HDUStart);

  declare
    Cards : Optional.Card_Arr := Read_Optional(F, Optional.Reserved.Reserved_Keys);
    Key : String(1..8);
  begin
    for I in Cards'Range
    loop
      Key := Cards(I)(1..8);
      if   (Key = "BZERO   ") then F64BZERO  := Float_64'Value(Cards(I)(11..30));
      elsif(Key = "BSCALE  ") then F64BSCALE := Float_64'Value(Cards(I)(11..30));
      end if;
    end loop;
  end;

  Set_File_Block_Index(F, HDUStart);

  declare
    HDUInfo : File.HDU_Info_Type := File.Read_Header(F);
    NAXISn  : NAXIS_Arr := HDUInfo.NAXISn;
    BITPIX  : Integer   := HDUInfo.BITPIX;
  begin
    DUStart := File_Block_Index(F);

    -- FIXME this case is weird - known at instantiation
    case(Tm'Size) is
      when 32 => Undef_Value := Tm(F32NaN);
      when 64 => Undef_Value := Tm(F32NaN);
      when others => null; -- Error
    end case;

    case(BITPIX) is
      when   8 => U8ReadIntVolume  (F, DUStart, NAXISn, First, Last, F64BZERO, F64BSCALE, Volume);
      when  16 => I16ReadIntVolume (F, DUStart, NAXISn, First, Last, F64BZERO, F64BSCALE, Volume);
      when -32 => F32F64ReadFloatVolume(F,DUStart,NAXISn, First,Last, F64BZERO,F64BSCALE, Undef_Value, Volume);
      when -64 => F64F64ReadFloatVolume(F,DUStart,NAXISn, First,Last, F64BZERO,F64BSCALE, Undef_Value, Volume);
      when others => null; -- FIXME Error
    end case;

  end;

end Read_Volume_As_Float;



-- Write,  BITPIX, Undef_Value, NAXISn are known

procedure Write_Array
  (F : in SIO.File_Type;
  Plane : in Tm_Arr)
is
begin
  null;
end Write_Array;




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


end V3_FITS_Image;

