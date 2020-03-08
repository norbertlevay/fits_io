
 with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;


-- NOTE data presents 3 orthogonal problems:
-- positioning by linear-index (data/unit)  vs coordintes (NCube)
-- sequential access by ICube's (I<N), random access of sub-Cube (maxi-coord < NAXISi)
-- data types (template programming, deferred to user inerface)



package Unit is

package SIO renames Ada.Streams.Stream_IO;


generic
  type T is private;
  type T_Arr is array (Positive_Count range <>) of T;
procedure Read_Array
  (F : SIO.File_Type;
   DUStart : in Positive_Count;      -- index in File of first Data-unit block
   First   : in Positive_Count := 1; -- index in DU of first data element to be read
   Values  : out T_Arr);




generic
  type T is private;
  type T_Arr is array (Positive_Count range <>) of T;
procedure Write_Array
   (F : SIO.File_Type;
   DUStart : in Positive_Count;       -- index in File of first Data-unit block
   First   : in Positive_Count := 1;  -- index in DU of first data element to be written
   Values  : in T_Arr);





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


-- float, check with 'Valid

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

end Unit;

