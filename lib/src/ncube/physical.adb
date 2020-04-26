
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
with Raw; use Raw;


package body Physical is

  use SIO;

  package TIO renames Ada.Text_IO;

  -- endianness

  generic
  type T is private;
  procedure Revert_Bytes( Data : in out T );
  procedure Revert_Bytes( Data : in out T )
  is
    Size_Bytes : Positive := T'Size / Interfaces.Unsigned_8'Size;
    type Arr4xU8 is array (1..Size_Bytes) of Interfaces.Unsigned_8;

    function Data_To_Arr is new Ada.Unchecked_Conversion(Source => T, Target => Arr4xU8);
    function Arr_To_Data is
      new Ada.Unchecked_Conversion(Source => Arr4xU8, Target => T);

    Arr  : Arr4xU8 := Data_To_Arr(Data);
    ArrO : Arr4xU8;
  begin

    for I in Arr'Range
    loop
      ArrO(I) := Arr(1 + Size_Bytes - I); 
    end loop;

    Data := Arr_To_Data(ArrO);

  end Revert_Bytes;



  -- Sequential access


  procedure Read_Int_Plane
    (F : SIO.File_Type;
    BZERO, BSCALE : in Tc;
    Length : in Positive_Count;
    Plane  : out Tm_Arr)
  is
    procedure RevertBytes is new Revert_Bytes(Tf);
    type Tf_Arr is array (Positive_Count range <>) of Tf;
    RawPlane : Tf_Arr(1..Length);
    procedure ReadRawPlane is new Read_Raw_Plane(Tf,Tf_Arr);
    function LinScale is new Unit.Scale(Tf,Tm,Tc, BZERO, BSCALE,"+","+");
  begin

    ReadRawPlane(F, RawPlane);

    for I in RawPlane'Range
    loop
      RevertBytes(RawPlane(I));
      Plane(I) := LinScale(RawPlane(I));
    end loop;

  end Read_Int_Plane;


  procedure Write_Int_Plane
    (F : SIO.File_Type;
    BZERO, BSCALE : in Tc;
--    Length : in Positive_Count;
    Plane  : in Tm_Arr)
  is
    procedure RevertBytes is new Revert_Bytes(Tf);
    type Tf_Arr is array (Positive_Count range <>) of Tf;
    RawPlane : Tf_Arr(1..Plane'Last);
    procedure WriteRawPlane is new Write_Raw_Plane(Tf,Tf_Arr);
    function LinScale is new Unit.Scale(Tf,Tm,Tc, BZERO, BSCALE,"+","+");
  begin

    for I in Plane'Range
    loop
      -- FIXME incorrect this is in Write(Tm->Tf) not Read(Tf->Tm):  RawPlane(I) := LinScale(Plane(I));
      RevertBytes(RawPlane(I));
    end loop;

    WriteRawPlane(F, RawPlane);

  end Write_Int_Plane;



  procedure Read_Float_Plane
    (F : SIO.File_Type;
    BZERO, BSCALE : in Tc;
    Length : in Positive_Count;
    Undef_Val : in Tm;
    Plane  : out Tm_Arr)
  is
    type Tf_Arr is array (Positive_Count range <>) of Tf;
    procedure ReadRawPlane is new Read_Raw_Plane(Tf,Tf_Arr);
    function LinFloatScale is new Unit.Scale_Float(Tf,Tm,Tc, BZERO, BSCALE, Undef_Val, "+","+");
    RawPlane : Tf_Arr(1..Length);
    procedure RevertBytes is new Revert_Bytes(Tf);
  begin

    ReadRawPlane(F, RawPlane);

    for I in RawPlane'Range
    loop
      RevertBytes(RawPlane(I));
      Plane(I) := LinFloatScale(RawPlane(I));
    end loop;

  end Read_Float_Plane;


  procedure Write_Float_Plane
    (F : SIO.File_Type;
    BZERO, BSCALE : in Tc;
    Length : in Positive_Count;
    Plane  : in Tm_Arr)
  is
    type Tf_Arr is array (Positive_Count range <>) of Tf;
    procedure WriteRawPlane is new Write_Raw_Plane(Tf,Tf_Arr);
    function LinFloatScale is new Unit.Scale_Float(Tf,Tm,Tc, BZERO, BSCALE, Undef_Val, "+","+");
    RawPlane : Tf_Arr(1..Length); -- FIXME convert and scale ? := Plane;
    procedure RevertBytes is new Revert_Bytes(Tf);
  begin

    for I in RawPlane'Range
    loop
      -- FIXME incorrect Write needs Reverse-LisnScale and Tm-> Tf RawPlane(I) := LinFloatScale(Plane(I));
      RevertBytes(RawPlane(I));
    end loop;

    WriteRawPlane(F, RawPlane);

  end Write_Float_Plane;





  -- Random access


  function Volume_Length
    (First : in NAXIS_Arr;
    Last   : in NAXIS_Arr) return Positive_Count
  is
    L : Positive_Count := 1;
  begin

    -- FIXME sanity check that First & Last are equal length

    for I in First'Range
    loop
      L := L * (1 + Positive_Count(Last(I) - First(I)));
    end loop;
    return L;
  end Volume_Length;



  procedure Read_Int_Volume
    (File : SIO.File_Type;
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    Last    : in NAXIS_Arr;
    BZERO, BSCALE : in Tc;
    Volume  : out Tm_Arr)
  is
    VolLength : Positive_Count := Volume_Length(First, Last);

    type Tf_Arr is array (Positive_Count range <>) of Tf;
    RawVol: Tf_Arr(1 .. VolLength);

    procedure ReadRawVolume is new Read_Raw_Volume(Tf,Tf_Arr);
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
    VolLength : Positive_Count := Volume_Length(First, Last);
    type Tf_Arr is array (Positive_Count range <>) of Tf;
    RawVol: Tf_Arr(1 .. VolLength);

    procedure ReadRawVolume is new Read_Raw_Volume(Tf,Tf_Arr);
    function LinScale is new Unit.Scale_Float(Tf,Tm,Tc, BZERO, BSCALE,Undef,"+","+");
  begin

    ReadRawVolume(File, DUStart, NAXISn, First, Last, RawVol);

    for I in RawVol'Range
    loop
      Volume(I) := LinScale(RawVol(I));
    end loop;

  end Read_Float_Volume;


end Physical;

