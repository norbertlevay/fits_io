
generic
type Tout is private;
type Tin  is private;
type Tc   is digits <>;
Undef_Val_In  : Tin;
Undef_Val_Out : Tout;
with function Is_Undef(Vin  : Tin ) return Boolean;
with function Found_Undef(Vin : Tin) return Tout;
with function Is_Undef(Vout : Tout) return Boolean;
with function in2c(Vin : Tin) return Tc;
with function c2out(Vc : Tc)  return Tout;
package Value is

function Linear(A,B : Tc; Vin : Tin) return Tout;



end Value;


