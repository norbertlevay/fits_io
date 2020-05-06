


package Linear_Conv is


-- convert to Floats

generic type Tin is digits <>; type Tc is digits <>; type Tout is digits <>; ToutNaN : Tout; function FF(A,B: Tc; Vin : Tin) return Tout;
generic type Tin is range  <>; type Tc is digits <>; type Tout is digits <>; ToutNaN : Tout; function FI_BLANK(BLANK: Tin; A,B: Tc; Vin : Tin) return Tout;
generic type Tin is mod    <>; type Tc is digits <>; type Tout is digits <>; ToutNaN : Tout; function FU_BLANK(BLANK: Tin; A,B: Tc; Vin : Tin) return Tout;
generic type Tin is range  <>; type Tc is digits <>; type Tout is digits <>;                 function FI(A,B: Tc; Vin : Tin) return Tout;
generic type Tin is mod    <>; type Tc is digits <>; type Tout is digits <>;                 function FU(A,B: Tc; Vin : Tin) return Tout;


-- convert to Ints

generic type Tin is digits <>; type Tc is digits <>; type Tout is range <>; function rF(A,B: Tc; Vin : Tin) return Tout;
generic type Tin is digits <>; type Tc is digits <>; type Tout is range <>; function rF_NaN(BLANK: Tout; A,B: Tc; Vin : Tin) return Tout;


-- convert to UInts



end Linear_Conv;
