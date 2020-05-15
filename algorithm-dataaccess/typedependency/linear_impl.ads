
with Header;


package Linear_Impl is

generic
type Tin  is private;
type Tout is private;
with function To_V3Type(Arg : String) return Tout is <>;
with function To_V3Type(Arg : String) return Tin is <>;
procedure From_Header(HData : in Header.Linear_Conv_Rec; A : out Tout; B : out Tout;
    BV : out Boolean; Undef : out Tin);



generic
type Tin  is private;
type Tout is private;
with function "+"(R : Tin) return Tout is <>;
with function "*"(L,R : Tout) return Tout is <>;
with function "+"(L,R : Tout) return Tout is <>;
function Linear_Pure(Vin : Tin; A,B:Tout) return Tout;


generic
type Tin  is private;
type Tout is private;
function Linear_Check_UndefIn(Vin : Tin; A,B:Tout) return Tout;


end Linear_Impl;

