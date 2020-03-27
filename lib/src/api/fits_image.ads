
package FITS_Image is

-- Read


generic
  type Tm is private;
procedure Read_Dimensions
  (F : in SIO.File_Type;
  BITPIX : out Integer;
  Undef_Value : out Tm;-- = Scaled(BLANK) or Float_NaN
  NAXISn : out NAXIS_Arr;) is null;


generic
  type Tm is private;
  type Tm_Arr is array (Positive_Count range <>) of Tm;
procedure Read_Plane
  (F : in SIO.File_Type;
  Length : in Positive_Count;-- ??
  Undef_Value : in Tm;
  Plane : out Tm_Arr) is null;


generic
  type Tm is private;
  type Tm_Arr is array (Positive_Count range <>) of Tm;
procedure Read_Volume
  (F : in SIO.File_Type;
  HDUStart : in Positive_Count;
  NAXISn : in NAXIS_Arr;-- ??
  First, Last : in NAXIS_Arr;
  Undef_Value : in Tm;
  Plane : out Tm_Arr) is null;



-- Write,  BITPIX, Undef_Value, NAXISn are known

generic
  type Tm is private;
  type Tm_Arr is array (Positive_Count range <>) of Tm;
procedure Write_Plane
  (F : in SIO.File_Type;
  Plane : in Tm_Arr) is null;


generic
  type Tm is private;
  type Tm_Arr is array (Positive_Count range <>) of Tm;
procedure Write_Volume
  (F : in SIO.File_Type;
  HDUStart : in Positive_Count;
  NAXISn : in NAXIS_Arr;-- ??
  First, Last : in NAXIS_Arr;
  Plane : in Tm_Arr) is null;


end FITS_Image;

