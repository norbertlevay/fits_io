
-- this hoes with Tc_Pool & Tc_Physical


package Conv_Impl is



generic
type Tin  is private;
type Tout is private;
with function "+"(R : Tin) return Tout is <>;
function Conv_Pure(Vin : Tin; BV:Boolean; UndefIn : Tin; UndefOut : Tout ) return Tout;
-- use for: FF II UU UI IU and IFnoBLANK

generic
type Tin  is private;
type Tout is private;
with function Is_Undef(Vin : in Tin) return Boolean is <>;
with function Do_Undef(Vout : in Tout) return Tout is <>;
with function "+"(R : Tin) return Tout is <>;
function Conv_Check_UndefIn(Vin : Tin; BV:Boolean; UndefIn : Tin; UndefOut : Tout ) return Tout;
-- use for: IFwBLANK

generic
type Tin  is private;
type Tout is private;
with function Is_Undef(Vin : in Tin) return Boolean is <>;
with function Do_Undef(Vout : in Tout) return Tout is <>;
with function "+"(R : Tin) return Tout is <>;
function Conv_Check_UndefOut(Vin : Tin; BV:Boolean; UndefIn : Tin; UndefOut : Tout ) return Tout;
-- use for: FIwBLANKout (BLANKout must exist)
-- if BLANKout exist, check that no valid Vin results in BLANKout



generic
type Tin  is private;
function Is_Undef_BLANK(Vin : Tin) return Boolean;

generic
type Tin  is private;
function Is_Undef_NaN(Vin : Tin) return Boolean;


generic
type Tout  is private;
function Do_Undef_BLANK(Vout : Tout) return Tout;


end Conv_Impl;

