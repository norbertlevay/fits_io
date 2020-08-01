


generic
type Tm is private;   -- type in memory
type Tc is digits <>; -- type in which scaling is calculated
type Tf is private;   -- type in fits-file

with function Init_UOut(UInValid : in Boolean; UIn : in Tf; UOutValid : in out Boolean; UOut : in out Tm) return Boolean is <>;

with function Is_Undef(V,U : Tf; UValid : Boolean) return Boolean is <>; 
with function Is_Undef(V,U : Tm; UValid : Boolean) return Boolean is <>; 

with function "+"(R : Tf) return Tc is <>; 
with function "+"(R : Tc) return Tm is <>; 


package Value is

 UIn  : Tf;-- = BLANK$
 UInValid  : Boolean := False;

    A,B : Tc;

--procedure Init(BZERO, BSCALE, BLANK : in String);


procedure Init_Undef
    (UInValid : in Boolean; UIn : in Tf; 
    UOutValid : in out Boolean; UOut : in out Tm);



function Scaling(Vin : Tf) return Tm;


end Value;
