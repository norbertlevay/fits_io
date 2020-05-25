
with Header;

generic
type Tm is private;
type Tf is private;
with procedure AB_Header_Info(HData : in Header.Linear_Conv_Rec;
                           A : out Tm; B: out Tm) is <>;
with procedure BLANK_Header_Info(HData : in Header.Linear_Conv_Rec;
                           BV : out Boolean; UndefIn : out Tf) is <>;
with function Linear(Vin : in Tf; A,B:Tm) return Tm is <>;
package Physical is


procedure Conv_Header
    (HData : in Header.Linear_Conv_Rec;
    A : out Tm; B: out Tm;
    BV : out Boolean;
    UndefIn : out Tf);


procedure Read_Array(dummy : Integer);

end Physical;

