


package body Physical is

    AA, BB : Tm;

procedure Conv_Header
    (HData : in Header.Linear_Conv_Rec;
    A : out Tm; B: out Tm;
    BV : out Boolean;
    UndefIn : out Tf; UndefOut : out Tm)
is
begin
    Header_Info(HData, A,B, BV, UndefIn, UndefOut);
end Conv_Header;




procedure Read_Array(dummy : Integer)
is
    Vin  : Tf;
    Vout : Tm;
begin
    Vout := Linear(Vin,AA,BB);
end Read_Array;



end Physical;
