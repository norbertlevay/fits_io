
with Header;


package Linear_Impl is

generic
type Tin  is private;
type Tout is private;
procedure From_Header(HData : in Header.Linear_Conv_Rec; A : out Tout; B : out Tout;
    BV : out Boolean; UndefIn : out Tin; UndefOut : out Tout);



generic
type Tin  is private;
type Tout is private;
function Linear_Pure(Vin : Tin; A,B:Tout) return Tout;


generic
type Tin  is private;
type Tout is private;
function Linear_Check_UndefIn(Vin : Tin; A,B:Tout) return Tout;


end Linear_Impl;

