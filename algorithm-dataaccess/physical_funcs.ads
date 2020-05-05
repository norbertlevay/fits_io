

package Physical_Funcs is



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
 function Scale_Float(Vf : Tf) return Tm;



end Physical_Funcs;

