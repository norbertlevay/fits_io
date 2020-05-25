
with Header;


package Linear_Impl is


generic
type Tout is private;
with function To_V3Type(Arg : String) return Tout is <>;
procedure AB_From_Header(HData : in Header.Linear_Conv_Rec; A : out Tout; B : out Tout);


generic
type Tin  is private;
with function To_V3Type(Arg : String) return Tin is <>;
procedure BLANK_From_Header(HData : in Header.Linear_Conv_Rec; BV : out Boolean; Undef : out Tin);


generic
type Tin  is private;
type Tout is private;
with function "+"(R : Tin) return Tout is <>;
with function "*"(L,R : Tout) return Tout is <>;
with function "+"(L,R : Tout) return Tout is <>;
function Linear_Pure(Vin : Tin; A,B:Tout) return Tout;
-- use for: FF II UU UI IU and IFnoBLANK

generic
type Tin  is private;
type Tout is private;
with function Is_Undef(Vin : in Tin) return Boolean is <>;
with function Do_Undef(Vout : in Tout) return Tout is <>;
with function "+"(R : Tin) return Tout is <>;
with function "*"(L,R : Tout) return Tout is <>;
with function "+"(L,R : Tout) return Tout is <>;
function Linear_Check_UndefIn(Vin : Tin; A,B:Tout) return Tout;
-- use for: IFwBLANK and FIwBLANKout (BLANKout must exist)
-- if BLANKout exist, check that no valid Vin results in BLANKout


generic
type Tin  is private;
function Is_Undef_BLANK(Vin : Tin) return Boolean;

generic
type Tout  is private;
function Do_Undef_BLANK(Vout : Tout) return Tout;


end Linear_Impl;

