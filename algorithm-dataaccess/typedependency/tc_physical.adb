
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
    Vcin  : Tc;
    Vcout : Tc;
    UV   : Boolean := False;
    iUIn  : Tf;
    iUOut : Tc;
    oUIn  : Tc;
    oUOut : Tm;
begin
    TIO.Put_Line("Tc_Physical::Read_Array");

    -- Conv() funcs must correctly convert also BLANK <-> NaN

    Vcin  := ConvIn(Vin,   UV, iUIn, iUOut);
    -- if Vin is Undef -> Vout := UndefOut
    -- if Vin is not Undef -> check: Vout is not UndefOut 
    --     -> if yes, raise Except: "valid value results in UndefOut"

    Vcout := AA + BB * Vcin;

    Vout  := ConvOut(Vcout, UV, oUIn, oUOut);


end Read_Array;



end Tc_Physical;
