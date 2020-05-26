
with Header;

generic
type Tm is private;
type Tc is private;
type Tf is private;
with procedure AB_Header_Info(HData : in Header.Linear_Conv_Rec;
                           A : out Tm; B: out Tm) is <>;
with procedure BLANK_Header_Info(HData : in Header.Linear_Conv_Rec;
                           BV : out Boolean; UndefIn : out Tf) is <>;
with function ConvIn (Vin : in Tf; BV:Boolean; UndefIn : Tf; UndefOut : Tc) return Tc is <>;
with function ConvOut(Vin : in Tc; BV:Boolean; UndefIn : Tc; UndefOut : Tm) return Tm is <>;
with function "*"(L,R : Tc) return Tc is <>; 
with function "+"(L,R : Tc) return Tc is <>; 
package Tc_Physical is


procedure Conv_Header
    (HData : in Header.Linear_Conv_Rec;
    A : out Tm; B: out Tm;
    BV : out Boolean;
    UndefIn : out Tf);


procedure Read_Array(AA,BB : Tc);

end Tc_Physical;

