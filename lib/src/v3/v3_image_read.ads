
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed
with Mandatory; use Mandatory;-- NAXIS_Arr needed
with Optional; -- Card_Arr needed
with V3_Types; use V3_Types;

generic
type Tm is private;
type Tm_Arr is array (Positive_Count range <>) of Tm;
type Tc is digits <>;

with function Init_UOut(UInValid : in Boolean; UIn : in Float_64; UOutValid : in out Boolean; UOut : in out Tm) return Boolean is <>;
with function Init_UOut(UInValid : in Boolean; UIn : in Float_32; UOutValid : in out Boolean; UOut : in out Tm) return Boolean is <>;
with function Init_UOut(UInValid : in Boolean; UIn : in Integer_64; UOutValid : in out Boolean; UOut : in out Tm) return Boolean is <>;
with function Init_UOut(UInValid : in Boolean; UIn : in Integer_32; UOutValid : in out Boolean; UOut : in out Tm) return Boolean is <>;
with function Init_UOut(UInValid : in Boolean; UIn : in Integer_16; UOutValid : in out Boolean; UOut : in out Tm) return Boolean is <>;
with function Init_UOut(UInValid : in Boolean; UIn : in Unsigned_8; UOutValid : in out Boolean; UOut : in out Tm) return Boolean is <>;


with function Is_Undef(V,U : Float_64; UValid : Boolean) return Boolean is <>;
with function Is_Undef(V,U : Float_32; UValid : Boolean) return Boolean is <>;
with function Is_Undef(V,U : Integer_64; UValid : Boolean) return Boolean is <>;
with function Is_Undef(V,U : Integer_32; UValid : Boolean) return Boolean is <>;
with function Is_Undef(V,U : Integer_16; UValid : Boolean) return Boolean is <>;
with function Is_Undef(V,U : Unsigned_8; UValid : Boolean) return Boolean is <>;
with function Is_Undef(V,U : Tm; UValid : Boolean) return Boolean is <>;



with function "+"(R : Float_64) return Tc is <>;
with function "+"(R : Float_32) return Tc is <>;
with function "+"(R : Integer_64) return Tc is <>;
with function "+"(R : Integer_32) return Tc is <>;
with function "+"(R : Integer_16) return Tc is <>;
with function "+"(R : Unsigned_8) return Tc is <>;

with function "+"(R : Tc) return Tm is <>;

with function To_V3Type(S : String) return Tm is <>;
--with function To_V3Type(S : String) return Tc is <>;

package V3_Image_Read is

  package SIO renames Ada.Streams.Stream_IO;


procedure Read_Volume
    (F : in SIO.File_Type;
    DUStart : in SIO.Positive_Count;
    BITPIX  : in Integer;
    NAXISn  : in NAXIS_Arr;
    First, Last : in NAXIS_Arr;
    Volume : out Tm_Arr;
    Cards  : in Optional.Card_Arr);


procedure Write_Volume
    (F : in SIO.File_Type;
    HDUStart : in Positive_Count;
    First, Last : in NAXIS_Arr;
    NAXISn : in NAXIS_Arr;
    Volume : in Tm_Arr);



generic
with procedure Plane_Data(Plane : in Tm_Arr; PlaneCount : in Positive_Count);
procedure Read_Data_Unit_By_Planes
    (F : in SIO.File_Type;
    BITPIX : in Integer;
    I      : in Positive; -- Plane max dimension I <= NAXIS
    NAXISn : in NAXIS_Arr;
    Undef_Value : in out Tm;
    Undef_Valid : in out Boolean;
    Cards  : in Optional.Card_Arr);
-- Tm_Arr has size NAXIS1 .. NAXISi, where i<=NAXISn'Length


generic
with procedure Plane_Data(Plane : out Tm_Arr; PlaneCount : in Positive_Count);
procedure Write_Data_Unit_By_Planes
    (F : in SIO.File_Type;
    NAXISi : in NAXIS_Arr); -- Tm_Arr has size NAXIS1 .. NAXISi, where i<=NAXISn'Length


end V3_Image_Read;
