
 -- FIXME  later make T_Arr private and also include
 -- NAXISn BITPIX and BLANK/UndefValue

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Mandatory; use Mandatory; -- NAXIS_Arr needed


package Physical is

 package SIO renames Ada.Streams.Stream_IO;

-- int, no need for Validity check: all bit-patterns
-- are valid nunmbers, ergo computation/scaling can be safely
-- performed also for BLANK which yields new BLANK in Tm

 generic
    type Tf is private;
    type Tm is private;
    type Tc is digits <>;
    BZERO  : in Tc;
    BSCALE : in Tc;
    with function "+"(R : in Tc) return Tm is <>;
    with function "+"(R : in Tf) return Tc is <>;
 function Scale(Vf : Tf) return Tm;


-- float, check for NaN

 generic
    type Tf is digits <>;
    type Tm is private;
    type Tc is digits <>;
    BZERO  : in Tc;
    BSCALE : in Tc;
    Undef  : in Tm; -- returns this when Vf invalid
    with function "+"(R : in Tc) return Tm is <>;
    with function "+"(R : in Tf) return Tc is <>;
 function Scale_Float(Vf : Tf) return Tm;


-- sequential access Read


 generic
  type Tf is private;
  type Tm is private;
  type Tm_Arr is array (Positive_Count range <>) of Tm;
  type Tc is digits <>;
  with function "+"(R : in Tf) return Tc is <>;
  with function "+"(R : in Tc) return Tm is <>;
 procedure Read_Int_Array
   (F : SIO.File_Type;
    BZERO, BSCALE : in Tc;
    Data : out Tm_Arr);


 generic
  type Tf is digits <>;
  type Tm is private;
  type Tm_Arr is array (Positive_Count range <>) of Tm;
  type Tc is digits <>;
  with function "+"(R : in Tf) return Tc is <>;
  with function "+"(R : in Tc) return Tm is <>;
 procedure Read_Float_Array
   (F : SIO.File_Type;
    BZERO, BSCALE : in Tc;
    Undef_Val : in Tm; -- FIXME reconsider: is this needed: for Float known per definition (=NaN)
    Data : out Tm_Arr);



-- sequentional access Write

 generic
  type Tf is private;
  type Tm is private;
  type Tm_Arr is array (Positive_Count range <>) of Tm;
  type Tc is digits <>;
  with function "+"(R : in Tc) return Tm is <>;
  with function "+"(R : in Tf) return Tc is <>;
 procedure Write_Int_Array
   (F : SIO.File_Type;
    BZERO, BSCALE : in Tc;
    Data : in Tm_Arr);


 generic
  type Tf is digits <>;
  type Tm is private;
  type Tm_Arr is array (Positive_Count range <>) of Tm;
  type Tc is digits <>;
  Undef_Val : in Tm;
  with function "+"(R : in Tf) return Tc is <>;
  with function "+"(R : in Tc) return Tm is <>;
 procedure Write_Float_Array
   (F : SIO.File_Type;
    BZERO, BSCALE : in Tc;
    Data : in Tm_Arr);




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
    Volume  : out Tm_Arr);-- FIXME Volume'Length must match with (Last - First)


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
    Undef   : in Tm; -- FIXME reconsider: known at instantiation (NaN)
    Volume  : out Tm_Arr);-- FIXME Volume'Length must match with (Last - First)


end Physical;

