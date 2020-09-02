

-- implements  Vout = A + B * Vin


generic
type Tout is private;   -- type in memory
type Tc is digits <>; -- type in which scaling is calculated
type Tin is private;   -- type in fits-file

with function Init_UOut(UInValid : in Boolean; UIn : in Tin; UOutValid : in out Boolean; UOut : in out Tout) return Boolean is <>;

with function Is_Undef(V,U : Tin; UValid : Boolean) return Boolean is <>;
with function Is_Undef(V,U : Tout; UValid : Boolean) return Boolean is <>;

with function "+"(R : Tin) return Tc is <>;
with function "+"(R : Tc) return Tout is <>;


package Value is

 UIn  : Tin;-- = BLANK$
 UInValid  : Boolean := False;

    A,B : Tc;

--procedure Init(BZERO, BSCALE, BLANK : in String);


procedure Init_Undef
    (UInValid : in Boolean; UIn : in Tin;
    UOutValid : in out Boolean; UOut : in out Tout);



function Scaling(Vin : Tin) return Tout;


end Value;
