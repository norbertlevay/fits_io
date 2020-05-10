



package Linear_Private is



generic
type Tc   is digits <>;
A,B : in out Tc;
type Tin  is private;
type Tout is private;
with function Is_Undef(Vin : in Tin) return Boolean;
with function Handle_Undef(Vin : in Tin) return Tout;
with function "+"(R : Tin) return Tc;
with function "+"(R : Tc) return Tout;
function Linear(Vin : in Tin) return Tout;

end Linear_Private;


