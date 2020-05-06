
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed
with Mandatory; use Mandatory;-- NAXIS_Arr needed

with V3_Types; use V3_Types;

generic
type Tm is digits <>;
type Tm_Arr is array (Positive_Count range <>) of Tm;
type Tcalc is digits <>;
package V3_Floats_Image is

  package SIO renames Ada.Streams.Stream_IO;



generic
TmNaN : Tm;
procedure Read_Volume
    (F : in SIO.File_Type;          -- File F
    HDUStart : in Positive_Count;   -- at offset FDUStart (referenced by HDUNum)
    First, Last : in NAXIS_Arr;     -- F and L point limiting the subcube of NAXISn
    Undef_Value : out Tm;
    Volume : out Tm_Arr);


procedure Write_Volume
    (F : in SIO.File_Type;
    HDUStart : in Positive_Count;
    NAXISn : in NAXIS_Arr;
    First, Last : in NAXIS_Arr;
    Volume : in Tm_Arr);



generic
with procedure Plane_Data(Plane : in Tm_Arr; PlaneCount : in Positive_Count);
procedure Read_Data_Unit_By_Planes
    (F : in SIO.File_Type;
    NAXISi : in NAXIS_Arr; -- Tm_Arr has size NAXIS1 .. NAXISi, where i<=NAXISn'Length
    Undef_Value : out Tm); -- FIXME how to return "none"=no BLANK (for raw Integers data)


generic
with procedure Plane_Data(Plane : out Tm_Arr; PlaneCount : in Positive_Count);
procedure Write_Data_Unit_By_Planes
    (F : in SIO.File_Type;
    NAXISi : in NAXIS_Arr; -- Tm_Arr has size NAXIS1 .. NAXISi, where i<=NAXISn'Length
    Undef_Value : in Tm); -- FIXME how to return "none"=no BLANK (for raw Integers data)


end V3_Floats_Image;

