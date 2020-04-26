
 -- FIXME  later make T_Arr private and also include
 -- NAXISn BITPIX and BLANK/UndefValue

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Mandatory; use Mandatory; -- NAXIS_Arr needed

with Raw; use Raw;



package NCube is

 package SIO renames Ada.Streams.Stream_IO;



-- sequential access Read


 generic
  type Tf is private;
  type Tm is private;
  type Tm_Arr is array (Positive_Count range <>) of Tm;
  type Tc is digits <>;
  with function "+"(R : in Tf) return Tc is <>;
  with function "+"(R : in Tc) return Tm is <>;
 procedure Read_Int_Plane
   (F : SIO.File_Type;
    BZERO, BSCALE : in Tc;
    Length : in Positive_Count;
    Plane  : out Tm_Arr);


 generic
  type Tf is digits <>;
  type Tm is private;
  type Tm_Arr is array (Positive_Count range <>) of Tm;
  type Tc is digits <>;
  with function "+"(R : in Tf) return Tc is <>;
  with function "+"(R : in Tc) return Tm is <>;
 procedure Read_Float_Plane
   (F : SIO.File_Type;
    BZERO, BSCALE : in Tc;
    Length : in Positive_Count;
    Undef_Val : in Tm;
    Plane  : out Tm_Arr);



-- sequentional access Write

 generic
  type Tf is private;
  type Tm is private;
  type Tm_Arr is array (Positive_Count range <>) of Tm;
  type Tc is digits <>;
  with function "+"(R : in Tc) return Tm is <>;
  with function "+"(R : in Tf) return Tc is <>;
 procedure Write_Int_Plane
   (F : SIO.File_Type;
    BZERO, BSCALE : in Tc;
--    Length : in Positive_Count;
    Plane  : in Tm_Arr);


 generic
  type Tf is digits <>;
  type Tm is private;
  type Tm_Arr is array (Positive_Count range <>) of Tm;
  type Tc is digits <>;
  Undef_Val : in Tm;
  with function "+"(R : in Tf) return Tc is <>;
  with function "+"(R : in Tc) return Tm is <>;
 procedure Write_Float_Plane
   (F : SIO.File_Type;
    BZERO, BSCALE : in Tc;
    Length : in Positive_Count;
    Plane  : in Tm_Arr);




-- random access

 generic
  type Tf is private;
  type Tm is private;
  type Tm_Arr is array (Positive_Count range <>) of Tm;
  type Tc is digits <>;
  with function "+"(R : in Tc) return Tm is <>;
  with function "+"(R : in Tf) return Tc is <>;
 procedure Read_Int_Volume
   (File : SIO.File_Type;
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    Last    : in NAXIS_Arr;
    BZERO, BSCALE : in Tc;
    Volume  : out Tm_Arr);


 generic
  type Tf is digits <>;
  type Tm is private;
  type Tm_Arr is array (Positive_Count range <>) of Tm;
  type Tc is digits <>;
  with function "+"(R : in Tc) return Tm is <>;
  with function "+"(R : in Tf) return Tc is <>;
 procedure Read_Float_Volume
   (File : SIO.File_Type;
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    Last    : in NAXIS_Arr;
    BZERO, BSCALE : in Tc;
    Undef   : in Tm;
    Volume  : out Tm_Arr);


end NCube;

