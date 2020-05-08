


package Linear_Conv is


-- convert to Floats

generic type Tin is digits <>; type Tc is digits <>; type Tout is digits <>;             A,B: Tc; ToutNaN : Tout; function FF(Vin : Tin) return Tout;
generic type Tin is range  <>; type Tc is digits <>; type Tout is digits <>; BLANK: Tin; A,B: Tc; ToutNaN : Tout; function FI_BLANK(Vin : Tin) return Tout;
generic type Tin is mod    <>; type Tc is digits <>; type Tout is digits <>; BLANK: Tin; A,B: Tc; ToutNaN : Tout; function FU_BLANK(Vin : Tin) return Tout;
generic type Tin is range  <>; type Tc is digits <>; type Tout is digits <>;             A,B: Tc;                 function FI(Vin : Tin) return Tout;
generic type Tin is mod    <>; type Tc is digits <>; type Tout is digits <>;             A,B: Tc;                 function FU(Vin : Tin) return Tout;


-- convert to Ints

generic type Tin is digits <>; type Tc is digits <>; type Tout is range <>;              A,B: Tc; function rF(Vin : Tin) return Tout;
generic type Tin is digits <>; type Tc is digits <>; type Tout is range <>; BLANK: Tout; A,B: Tc; function rF_NaN(Vin : Tin) return Tout;


-- convert to UInts



end Linear_Conv;
