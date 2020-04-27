
-- Sequential Access:
-- if T_Arr'Length = NAXISi*NAXISi-1*...*NAXIS1
-- then repeating Read (NAXISn*NAXISn-1*...NAXISi+1)-times 
-- reads all DU sequentially
-- NOTE position to DUStart before 1st call in Read/Write_x_Plane

with Ada.Text_IO;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;
with Interfaces;

with Data_Unit;
with Unit;
with Data_Funcs;    use Data_Funcs;
with Mandatory;     use Mandatory; -- NAXIS_Arr needed
with Keyword_Record; use Keyword_Record; -- FIndex needed in NAXIS_Arr

with NCube_Funcs; use NCube_Funcs;
with Raw;


package body Physical is

  use SIO;

  package TIO renames Ada.Text_IO;


  -- Sequential access


  procedure Read_Int_Plane
    (F : SIO.File_Type;
    BZERO, BSCALE : in Tc;
    Plane  : out Tm_Arr)
  is
    type Tf_Arr is array (Positive_Count range <>) of Tf;
    RawPlane : Tf_Arr(Plane'First .. Plane'Last);
    procedure ReadRawPlane is new Raw.Read_Plane(Tf,Tf_Arr);
    function LinScale is new Unit.Scale(Tf,Tm,Tc, BZERO, BSCALE,"+","+");
  begin
    ReadRawPlane(F, RawPlane);
  end Read_Int_Plane;


  procedure Write_Int_Plane
    (F : SIO.File_Type;
    BZERO, BSCALE : in Tc;
    Plane  : in Tm_Arr)
  is
    type Tf_Arr is array (Positive_Count range <>) of Tf;
    RawPlane : Tf_Arr(1..Plane'Last);
    procedure WriteRawPlane is new Raw.Write_Plane(Tf,Tf_Arr);
    function LinScale is new Unit.Scale(Tf,Tm,Tc, BZERO, BSCALE,"+","+");
  begin
    -- FIXME incorrect this is in Write(Tm->Tf) not Read(Tf->Tm):
    -- RawPlane(I) := LinScale(Plane(I));
    WriteRawPlane(F, RawPlane);
  end Write_Int_Plane;



  procedure Read_Float_Plane
    (F : SIO.File_Type;
    BZERO, BSCALE : in Tc;
    Undef_Val : in Tm;
    Plane  : out Tm_Arr)
  is
    type Tf_Arr is array (Positive_Count range <>) of Tf;
    procedure ReadRawPlane is new Raw.Read_Plane(Tf,Tf_Arr);
    function LinFloatScale is new Unit.Scale_Float(Tf,Tm,Tc, BZERO, BSCALE, Undef_Val, "+","+");
    RawPlane : Tf_Arr(Plane'First .. Plane'Last);
  begin
    ReadRawPlane(F, RawPlane);
    -- FIXME verify this
    for I in RawPlane'Range
    loop
      Plane(I) := LinFloatScale(RawPlane(I));
    end loop;
  end Read_Float_Plane;


  procedure Write_Float_Plane
    (F : SIO.File_Type;
    BZERO, BSCALE : in Tc;
    Plane  : in Tm_Arr)
  is
    type Tf_Arr is array (Positive_Count range <>) of Tf;
    procedure WriteRawPlane is new Raw.Write_Plane(Tf,Tf_Arr);
    function LinFloatScale is new Unit.Scale_Float(Tf,Tm,Tc, BZERO, BSCALE, Undef_Val, "+","+");
    RawPlane : Tf_Arr(Plane'First .. Plane'Last); -- FIXME convert and scale ? := Plane;
  begin
    for I in RawPlane'Range
    loop
      -- FIXME incorrect Write needs Reverse-LisnScale and Tm
      -- -> Tf RawPlane(I) := LinFloatScale(Plane(I));
      null;
    end loop;
    WriteRawPlane(F, RawPlane);
  end Write_Float_Plane;





  -- Random access




  procedure Read_Int_Volume
    (File : SIO.File_Type;
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    Last    : in NAXIS_Arr;
    BZERO, BSCALE : in Tc;
    Volume  : out Tm_Arr)
  is
    VolLength : Positive_Count := Raw.Volume_Length(First, Last);

    type Tf_Arr is array (Positive_Count range <>) of Tf;
    RawVol: Tf_Arr(1 .. VolLength);

    procedure ReadRawVolume is new Raw.Read_Volume(Tf,Tf_Arr);
    function LinScale is new Unit.Scale(Tf,Tm,Tc, BZERO, BSCALE,"+","+");
  begin

    ReadRawVolume(File, DUStart, NAXISn, First, Last, RawVol);

    for I in RawVol'Range
    loop
      Volume(I) := LinScale(RawVol(I));
    end loop;

  end Read_Int_Volume;





  procedure Read_Float_Volume
    (File : SIO.File_Type;
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    Last    : in NAXIS_Arr;
    BZERO, BSCALE : in Tc;
    Undef   : in Tm;
    Volume  : out Tm_Arr)
  is
    VolLength : Positive_Count := Raw.Volume_Length(First, Last);
    type Tf_Arr is array (Positive_Count range <>) of Tf;
    RawVol: Tf_Arr(1 .. VolLength);

    procedure ReadRawVolume is new Raw.Read_Volume(Tf,Tf_Arr);
    function LinScale is new Unit.Scale_Float(Tf,Tm,Tc, BZERO, BSCALE,Undef,"+","+");
  begin

    ReadRawVolume(File, DUStart, NAXISn, First, Last, RawVol);

    for I in RawVol'Range
    loop
      Volume(I) := LinScale(RawVol(I));
    end loop;

  end Read_Float_Volume;


end Physical;

