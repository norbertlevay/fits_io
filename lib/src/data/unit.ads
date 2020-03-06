
 with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;


package Unit is

package SIO renames Ada.Streams.Stream_IO;



generic
  type T is private;
  with procedure Element (V : in T);
procedure Read_Array_Values
   (F : SIO.File_Type;
    Length : in Positive_Count;
    First  : in Positive := 1);

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

