


package Linear_Conv is


generic type Tin is digits <>; type Tc is digits <>; type Tout is digits <>; ToutNaN : Tout;  function FF(A,B: Tc; Vin : Tin) return Tout;
generic type Tin is range  <>; type Tc is digits <>; type Tout is digits <>; function FI(A,B: Tc; Vin : Tin) return Tout;
generic type Tin is mod    <>; type Tc is digits <>; type Tout is digits <>; function FU(A,B: Tc; Vin : Tin) return Tout;

generic type Tin is digits <>; type Tc is digits <>; type Tout is range <>; function rF(A,B: Tc; Vin : Tin) return Tout;

end Linear_Conv;
