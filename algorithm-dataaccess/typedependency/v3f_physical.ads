
with V3_Types; use V3_Types;
with Header;

generic
type Tm is private;
with function "+"(R : Float_32) return Tm is <>;
with function "+"(R : Integer_16) return Tm is <>;
with function "*"(L,R : Tm) return Tm is <>;
with function "+"(L,R : Tm) return Tm is <>;
with procedure AB_Header_Info(HData : in Header.Linear_Conv_Rec;
                            A : out Tm; B: out Tm) is <>;
package V3f_Physical is


procedure Read_Array(Dummy:Integer);


end V3f_Physical;

