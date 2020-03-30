
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed or is there FITS.Positive_count?
with Mandatory; use Mandatory;-- NAXIS_Arr needed

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

procedure Read_Plane
  (F : in SIO.File_Type;
  Undef_Value : in Tm;   -- FIXME how to give "none" no BLANK provided (for raw Integers data)
  NAXISi : in NAXIS_Arr; -- dimensions of Tm_Arr  (i < NAXISn'Length):  Length of Plane Tm_Arr
  Plane : out Tm_Arr)
is
  BITPIX : Integer;
  NAXISn : NAXIS_Arr(1..1);-- FIXME

  procedure ReadIntPlane   is new Read_Int_Plane(Tf, Tm, Tm_Arr, Tc);
  procedure ReadFloatPlane is new Read_Float_Plane(Tf, Tm, Tm_Arr, Tc, Undefi_Value);
begin
  Read_Dimensions(F, BITPIX, NAXISn);

  if(BITPIX > 0)
  then
    -- call ReadIntPlane()
    null;
  else
    -- call ReadFloatPlane()
    null;
  end if;

end Read_Plane;


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

