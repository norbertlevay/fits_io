
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed or is there FITS.Positive_count?
with Mandatory; use Mandatory;-- NAXIS_Arr needed

with V3_Types; use V3_Types;


package FITS_Image is
  
  package SIO renames Ada.Streams.Stream_IO;


-- internal FIST_Image model: type, undefined value, dimensions

-- Read metadata

procedure Read_Dimensions
  (F : in SIO.File_Type;
  BITPIX : out Integer;
  NAXISn : out NAXIS_Arr);
-- NOTE comes from Mandatory-keys parsing

generic
  type Tm is private;
function Read_Undef_Value(F : in SIO.File_Type) return Tm;
-- returns Scaled(BLANK) or Float_NaN
-- NOTE comes from Optional-keys parsing

-- NOTE:
  --type Tm is (<>);--discrete
  --type Tm is mod <>;-- Unsigned_N
  --type Tm is range <>; -- Integer_N
  --type Tm is private;-- cant have because no conversion operator: for conc operator tyőe must be number
 
-- Read data
 -- Undef Values$
 F64NaN : constant Float_64 := Float_64(16#7FF0000000000100#);
 F32NaN : constant Float_32 := Float_32(16#7F800001#);

generic
  type Tm is digits <>;
  type Tm_Arr is array (Positive_Count range <>) of Tm;
procedure Read_Plane_As_Float
  (F : in SIO.File_Type;
  Undef_Value : in Tm;   -- FIXME how to give "none" no BLANK provided (for raw Integers data)
  NAXISi : in NAXIS_Arr; -- dimensions of Tm_Arr  (i < NAXISn'Length):  Length of Plane Tm_Arr
  Plane : out Tm_Arr);


generic
  type Tm is private;
  type Tm_Arr is array (Positive_Count range <>) of Tm;
procedure Read_Volume
  (F : in SIO.File_Type;          -- File F
  HDUStart : in Positive_Count;   -- at offset FDUStart (referenced by HDUNum)
  NAXISn : in NAXIS_Arr;          -- contains n-dimensional array (n=NAXISn'Length)
  Undef_Value : in Tm;
  First, Last : in NAXIS_Arr;     -- F and L point limiting the subcube of NAXISn
  Plane : out Tm_Arr);



-- Write,  BITPIX, Undef_Value, NAXISn are known

generic
  type Tm is private;
  type Tm_Arr is array (Positive_Count range <>) of Tm;
procedure Write_Plane
  (F : in SIO.File_Type;
  Plane : in Tm_Arr);


generic
  type Tm is private;
  type Tm_Arr is array (Positive_Count range <>) of Tm;
procedure Write_Volume
  (F : in SIO.File_Type;
  HDUStart : in Positive_Count;
  NAXISn : in NAXIS_Arr;
  First, Last : in NAXIS_Arr;
  Plane : in Tm_Arr);


end FITS_Image;

