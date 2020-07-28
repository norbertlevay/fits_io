

package Linear_Impl is



generic
type Tin  is private;
type Tcalc is digits <>;
type Tout is private;
with function "+"(R : Tin) return Tcalc is <>;
with function "+"(R : Tcalc) return Tout is <>;
function Linear_Pure(Vin : Tin; A,B:Tcalc; BV: Boolean; BLANK : Tin) return Tout;


generic
type Tin  is private;
type Tcalc is digits <>;
type Tout is digits <>;
UndefOut : in Tout;
with function "+"(R : Tin) return Tcalc is <>;
with function "+"(R : Tcalc) return Tout is <>;
function Linear_Check_UndefIn(Vin : Tin; A,B:Tcalc; BV: Boolean; BLANK : Tin) return Tout;

generic
type Tin  is digits <>;
type Tcalc is digits <>;
type Tout is private;
UndefOut : in Tout;
with function "+"(R : Tin) return Tcalc is <>;
with function "+"(R : Tcalc) return Tout is <>;
function Linear_Check_UndefOut(Vin : Tin; A,B:Tcalc; BV: Boolean; BLANK : Tin) return Tout;


-- Scaling variant


-- Impl1: with BLANK: FF UU II UI IU
-- (Check_InValue is not called when no BLANK)

 generic
 type Tf is private;
 type Tm is private;
 procedure Check_InValue_Null(Vin,UIn: in Tf; UOut: in Tm;
     Vout : in out Tm; OutValSet : in out Boolean);

 generic
 type Tf is private;
 type Tm is private;
 procedure Check_OutValue_Null(Vin,UIn: in Tf; Vout,UOut: in Tm);


-- Impl2: used only UI-> F, with BLANK
-- (Check_InValue is not called when no BLANK)

 generic
 type Tf is private;
 type Tm is private;
 UOutNaN : in Tm;
 procedure Check_InValue_BLANK(Vin,UIn: in Tf; UOut: in Tm;
     Vout : in out Tm; OutValSet : in out Boolean);


-- Impl3: used only F -> UI
-- (ignore BLANK)

 generic
 type Tf is private;
 type Tm is private;
 UOutUser : in Tm;
 procedure Check_InValue_F2UI(Vin,UIn: in Tf; UOut: in Tm;
     Vout : in out Tm; OutValSet : in out Boolean);

 generic
 type Tf is private;
 type Tm is private;
 UOutUser : in Tm;
 procedure Check_OutValue_F2UI(Vin,UIn: in Tf; Vout,UOut: in Tm);



end Linear_Impl;

