
with Optional;


package Linear_Impl is

generic
type Tin  is private;
type Tcalc is digits <>;
type Tout is private;
with function To_V3Type(Arg : String) return Tout is <>;
with function To_V3Type(Arg : String) return Tcalc is <>;
with function To_V3Type(Arg : String) return Tin is <>;
procedure From_Header(Cards : in Optional.Card_Arr; A,B : out Tcalc;
    BV : out Boolean; BLANK : out Tin);



generic
type Tin  is private;
type Tcalc is digits <>;
type Tout is private;
with function "+"(R : Tin) return Tcalc is <>;
with function "+"(R : Tcalc) return Tout is <>;
--with function "*"(L,R : Tout) return Tout is <>;
--with function "+"(L,R : Tout) return Tout is <>;
function Linear_Pure(Vin : Tin; A,B:Tcalc; BV: Boolean; BLANK : Tin) return Tout;


generic
type Tin  is private;
type Tcalc is digits <>;
type Tout is private;
function Linear_Check_UndefIn(Vin : Tin; A,B:Tcalc; BV: Boolean; BLANK : Tin) return Tout;


end Linear_Impl;

