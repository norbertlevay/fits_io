
-- Sequential Access:
-- if T_Arr'Length = NAXISi*NAXISi-1*...*NAXIS1
-- then repeating Read (NAXISn*NAXISn-1*...NAXISi+1)-times 
-- reads all DU sequentially
-- NOTE position to DUStart before 1st call in Read/Write_x_Data


-- NOTE we separate to Int and Float because Scale_Float checks 
-- that value is valid and Scale used fpr Int does not check validity.
-- Because in case of (U)Int, BLANK is in range of valid values
-- whereas for Floats NaN is invalid in Ada, woudl raise exception
-- for (U)Int:  new-BLANK = A + B * BLANK
-- for Float:   A + B * NaN -> raises exception

with Ada.Text_IO;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;
with Interfaces;

with Mandatory;     use Mandatory; -- NAXIS_Arr needed
with Keyword_Record; use Keyword_Record; -- FIndex needed in NAXIS_Arr
with NCube_Funcs; use NCube_Funcs;
with Physical_Funcs; use Physical_Funcs;
with Raw;


package body Floats_Physical is

  use SIO;

  package TIO renames Ada.Text_IO;



  -- Sequential access


  procedure Read_Array
    (F : SIO.File_Type;
    BZERO, BSCALE : in Tc;
    Undef_Val : in Tm;
    Data : out Tm_Arr)
  is
    type Tf_Arr is array (Positive_Count range <>) of Tf;
    package Tf_Raw is new Raw(Tf,Tf_Arr);
    function LinFloatScale is new Scale_Float(Tf,Tm,Tc, BZERO, BSCALE, Undef_Val, "+","+");
    RawData : Tf_Arr(Data'First .. Data'Last);
  begin
    Tf_Raw.Read_Array(F, RawData);
    for I in RawData'Range
    loop
      Data(I) := LinFloatScale(RawData(I));
    end loop;
  end Read_Array;


  procedure Write_Array
    (F : SIO.File_Type;
    BZERO, BSCALE : in Tc;
    Data : in Tm_Arr)
  is
    type Tf_Arr is array (Positive_Count range <>) of Tf;
    package Tf_Raw is new Raw(Tf,Tf_Arr);
    function LinFloatScale is new Scale_Float(Tf,Tm,Tc, BZERO, BSCALE, Undef_Val, "+","+");
    RawData : Tf_Arr(Data'First .. Data'Last); -- FIXME convert and scale ? := Plane;
  begin
    for I in RawData'Range
    loop
      -- FIXME incorrect Write needs Reverse-LisnScale and Tm
      -- -> Tf RawData(I) := LinFloatScale(Data(I));
      null;
    end loop;
    Tf_Raw.Write_Array(F, RawData);
  end Write_Array;





  -- Random access


  procedure Read_Volume
    (File : SIO.File_Type;
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    Last    : in NAXIS_Arr;
    BZERO, BSCALE : in Tc;
    Undef   : in Tm;
    Volume  : out Tm_Arr)
  is
    VolLength : Positive_Count := Volume_Length(First, Last);
    type Tf_Arr is array (Positive_Count range <>) of Tf;
    RawVol: Tf_Arr(1 .. VolLength);

    package Tf_Raw is new Raw(Tf,Tf_Arr);
    function LinScaleFloat is new Scale_Float(Tf,Tm,Tc, BZERO, BSCALE,Undef,"+","+");
  begin

    Tf_Raw.Read_Volume(File, DUStart, NAXISn, First, Last, RawVol);

    for I in RawVol'Range
    loop
      Volume(I) := LinScaleFloat(RawVol(I));
    end loop;

  end Read_Volume;


end Floats_Physical;

