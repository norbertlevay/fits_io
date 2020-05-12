



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



generic
type Tc   is digits <>;
A,B : in out Tc;
type Tin  is private;-- FIXME should I be able to say digits here? other maybe private-only
type Tout is private;
Undef_Valid   : in out Boolean;
Undef_Val_Out : in out Tout;
with function "+"(R : Tin) return Tc;
with function "+"(R : Tc) return Tout;
function Linear_From_Floats(Vin : in Tin) return Tout;
-- implements if (is_nan(vin)) check




generic
type Tc   is digits <>;
A,B : in out Tc;
type Tin  is private;
type Tout is private;
Undef_Valid   : in out Boolean;
Undef_Val_In  : in out Tin;
Undef_Val_Out : in out Tout;
with function "+"(R : Tin) return Tc;
with function "+"(R : Tc) return Tout;
function Linear_From_Ints(Vin : in Tin) return Tout;
-- implements if (vin = blankin) check




end Linear_Private;


