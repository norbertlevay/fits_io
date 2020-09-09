

-- implements  Vout = A + B * Vin
-- for all type combinations


generic
type Tout is private;   -- type of scaled output
type Tc   is digits <>; -- type in which scaling is calculated
type Tin  is private;   -- type of input

with function Init_UOut(UInValid :  in     Boolean; UIn  : in     Tin;
                        UOutValid : in out Boolean; UOut : in out Tout) return Boolean is <>;

with function Is_Undef(V,U : Tin;  UValid : Boolean) return Boolean is <>;
with function Is_Undef(V,U : Tout; UValid : Boolean) return Boolean is <>;

with function "+"(R : Tin) return Tc   is <>;
with function "+"(R : Tc ) return Tout is <>;


package Scaling is

 UIn      : Tin;
 UInValid : Boolean := False;

 A,B : Tc;



procedure Init_Undef
    (UInValid : in     Boolean; UIn  : in     Tin;
    UOutValid : in out Boolean; UOut : in out Tout);


function Linear(Vin : Tin) return Tout;


end Scaling;