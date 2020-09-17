



package body Scaling is


function Undefined(V : in Tsrc.Numeric) return Tdst.Numeric
is
    Uout : Tdst.Numeric;
    FUin  : Float;
    FUout : Float;
begin

    FUin := Tsrc.To_Float(V);
    FUout := A + B * FUin;
    Uout := Tdst.To_Numeric(FUout);

    return Uout;
end Undefined;


end Scaling;

