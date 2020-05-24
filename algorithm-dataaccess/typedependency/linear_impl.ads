
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


generic
type Tin  is private;
type Tout is private;
function Linear_Check_UndefIn(Vin : Tin; A,B:Tout) return Tout;


end Linear_Impl;

