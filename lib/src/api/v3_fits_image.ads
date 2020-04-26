
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed or is there FITS.Positive_count?
with Mandatory; use Mandatory;-- NAXIS_Arr needed

with V3_Types; use V3_Types;


package V3_FITS_Image is

  package SIO renames Ada.Streams.Stream_IO;

  -- Undef Values$
  F64NaN : constant Float_64 := Float_64(16#7FF0000000000100#);
  F32NaN : constant Float_32 := Float_32(16#7F800001#);


  -- Read metadata
  -- internal FIST_Image model: type, undefined value, dimensions

  procedure Read_Dimensions
    (F : in SIO.File_Type;
    BITPIX : out Integer;
    NAXISn : out NAXIS_Arr);

  generic
    type Tm is private;
  function Read_Undef_Value(F : in SIO.File_Type) return Tm;



-- Read data

  generic
    type Tm is digits <>;
    type Tm_Arr is array (Positive_Count range <>) of Tm;
  procedure Read_Plane_As_Float
    (F : in SIO.File_Type;
    Undef_Value : out Tm;   -- FIXME how to return "none"=no BLANK (for raw Integers data)
    I : in Positive; -- Tm_Arr has size NAXIS1 .. NAXISi, where i<=NAXISn'Length
    Plane : out Tm_Arr);


  generic
    type Tm is digits <>;
    type Tm_Arr is array (Positive_Count range <>) of Tm;
  procedure Read_Volume_As_Float
    (F : in SIO.File_Type;          -- File F
    HDUStart : in Positive_Count;   -- at offset FDUStart (referenced by HDUNum)
    First, Last : in NAXIS_Arr;     -- F and L point limiting the subcube of NAXISn
    Undef_Value : out Tm;
    Volume : out Tm_Arr);



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
    Volume : in Tm_Arr);


end V3_FITS_Image;

