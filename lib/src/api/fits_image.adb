
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
  procedure  U8ReadIntPlane is      new Read_Int_Plane(Unsigned_8,Tm,Tm_Arr,Tcalc,"+","+");
  procedure I16ReadIntPlane is      new Read_Int_Plane(Integer_16,Tm,Tm_Arr,Tcalc,"+","+");
  procedure F32F64ReadFloatPlane is new Read_Float_Plane(Float_32,Tm,Tm_Arr,Tcalc,"+","+");
  procedure F64F64ReadFloatPlane is new Read_Float_Plane(Float_64,Tm,Tm_Arr,Tcalc,"+","+");


begin
  Read_Dimensions(F, BITPIX, NAXISn);

  -- FIXME read BZERO BSCALE (BLANK)

  case(BITPIX) is
    when   8 => U8ReadIntPlane   (F, F64BZERO, F64BSCALE, Length, Plane);
    when  16 => I16ReadIntPlane  (F, F64BZERO, F64BSCALE, Length, Plane);
    when -32 => F32F64ReadFloatPlane(F, F64BZERO, F64BSCALE, Length, Undef_Value, Plane);
    when -64 => F64F64ReadFloatPlane(F, F64BZERO, F64BSCALE, Length, Undef_Value, Plane);
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

