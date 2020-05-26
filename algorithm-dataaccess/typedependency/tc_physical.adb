
with Ada.Text_IO;

package body Tc_Physical is

    package TIO renames Ada.Text_IO;

    AA, BB : Tm;

procedure Conv_Header
    (HData : in Header.Linear_Conv_Rec;
    A : out Tm; B: out Tm;
    BV : out Boolean;
    UndefIn : out Tf)
is
begin
    AB_Header_Info(HData, A,B);
    AA := A;
    BB := B;
    BLANK_Header_Info(HData, BV, UndefIn);
end Conv_Header;




procedure Read_Array(AA,BB : Tc)
is
    Vin  : Tf;
    Vout : Tm;
    BV : Boolean := False;
    UIn : Tf;
    UOut : Tm;
begin
    TIO.Put_Line("Tc_Physical::Read_Array");

--    Vout := Linear(Vin,AA,BB,BV,UIn,Uout);


    -- Conv() funcs must correctly convert also BLANK <-> NaN

    Vcin  := Conv(Vin, BV, UIn, UOut);

    Vcout := AA + BB * Vcin;

    Vout := Conv(Vcout, BV, UIn, UOut);


end Read_Array;



end Tc_Physical;
