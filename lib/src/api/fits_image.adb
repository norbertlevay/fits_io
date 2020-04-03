
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed or is there FITS.Positive_count?
with Mandatory; use Mandatory;-- NAXIS_Arr needed

with V3_Types; use V3_Types;-- types needed
with NCube; use NCube;

package body FITS_Image is

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

procedure Read_Plane_As_Float
  (F : in SIO.File_Type;
  Undef_Value : in Tm;   -- FIXME how to give "none" no BLANK provided (for raw Integers data)
  NAXISi : in NAXIS_Arr; -- dimensions of Tm_Arr  (i < NAXISn'Length):  Length of Plane Tm_Arr
  Plane : out Tm_Arr)
is
  BITPIX : Integer;
  NAXISn : NAXIS_Arr(1..1);-- FIXME
  Length : Positive_Count := 1; -- calc from NAXISn

  function "+"(R : in Unsigned_8) return Tm is begin return Tm(R); end "+";
  function "+"(R : in Integer_16) return Tm is begin return Tm(R); end "+";
  procedure  U8ReadIntPlane is new Read_Int_Plane(Unsigned_8, Tm, Tm_Arr, Tm, "+", "+");
  procedure I16ReadIntPlane is new Read_Int_Plane(Integer_16, Tm, Tm_Arr, Tm, "+", "+");

  function "+"(R : in Float_64) return Tm is Vm : Tm; begin return Tm(R);  end "+";
  function "+"(R : in Float_32) return Tm is Vm : Tm; begin return Tm(R);  end "+";
  procedure F64ReadFloatPlane is new Read_Float_Plane(Float_64,Tm,Tm_Arr,Float_64,"+","+");
  procedure F32ReadFloatPlane is new Read_Float_Plane(Float_32,Tm,Tm_Arr,Float_32,"+","+");

  F32BZERO, F32BSCALE : Float_32;
  F64BZERO, F64BSCALE : Float_64;

--  F64NaN : constant Float_64 := Float_64(16#7FF0000000000100#);
--  F32NaN : constant Float_32 := Float_32(16#7F800001#);

begin
  Read_Dimensions(F, BITPIX, NAXISn);

  -- FIXME read BZERO BSCALE (BLANK)

--  if(Tm'Size = 32) then Undef := Tm(F32NaN); end if;
--  if(Tm'Size = 64) then Undef := Tm(F64NaN); end if;

  case(BITPIX) is
    when   8 => U8ReadIntPlane   (F, Tm(F64BZERO), Tm(F64BSCALE), Length, Plane); -- FIXME Tm()
    when  16 => I16ReadIntPlane  (F, Tm(F64BZERO), Tm(F64BSCALE), Length, Plane); -- FIXME Tm()
    when -32 => F32ReadFloatPlane(F, F32BZERO, F32BSCALE, Length, Undef_Value, Plane);
    when -64 => F64ReadFloatPlane(F, F64BZERO, F64BSCALE, Length, Undef_Value, Plane);
    when others => null; -- FIXME Error
  end case;

end Read_Plane_As_Float;

-- note:
--  if(BITPIX > 0)
--  then
    -- call ReadIntPlane()
--    null;
--  else
    -- call ReadFloatPlane()
--    null;
--  end if;




procedure Read_Volume
  (F : in SIO.File_Type;          -- File F
  HDUStart : in Positive_Count;   -- at offset FDUStart (referenced by HDUNum)
  NAXISn : in NAXIS_Arr;          -- contains n-dimensional array (n=NAXISn'Length)
  Undef_Value : in Tm;
  First, Last : in NAXIS_Arr;     -- F and L point limiting the subcube of NAXISn
  Plane : out Tm_Arr)
is
begin
  null;
end Read_Volume;



-- Write,  BITPIX, Undef_Value, NAXISn are known

procedure Write_Plane
  (F : in SIO.File_Type;
  Plane : in Tm_Arr)
is
begin
  null;
end Write_Plane;




procedure Write_Volume
  (F : in SIO.File_Type;
  HDUStart : in Positive_Count;
  NAXISn : in NAXIS_Arr;
  First, Last : in NAXIS_Arr;
  Plane : in Tm_Arr)
is
begin
  null;
end Write_Volume;


end FITS_Image;

