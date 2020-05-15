
with Header;

generic
type Tm is private;
type Tf is private;
with procedure Header_Info(HData : in Header.Linear_Conv_Rec;A : out Tm; B: out Tm;
        BV : out Boolean; UndefIn : out Tf) is <>;--R/W swap Tf<->Tm
with function Linear(Vin : in Tf; A,B:Tm) return Tm is <>; --forRead,forWrite swap Tf<->Tm
package Physical is


procedure Conv_Header
    (HData : in Header.Linear_Conv_Rec;
    A : out Tm; B: out Tm;
    BV : out Boolean;
    UndefIn : out Tf);


procedure Read_Array(dummy : Integer);

end Physical;
