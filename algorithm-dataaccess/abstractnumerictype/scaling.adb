



package body Scaling is

-- calculates the Undefined value for the Tdst domain
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
-- above: applies pure-conversion

-- below: converts with substitution when Undef (if Undef defined)
function Is_Undef_Float(F : in Float) return Boolean
is
begin
    return (Not (F = F));
end Is_Undef_Float;

function Linear(V : in Tsrc.Numeric) return Tdst.Numeric
is
    Vout : Tdst.Numeric;
    Fin  : Float;
    Fout : Float;
    Zero : Float := 0.0; -- FIXME from ieee.adb - said to be NON-PORTABLE ???; define NaN values??
    NaN  : Float := 0.0/Zero;-- FIXME  define NaN values for various Float-types
begin

    if(Tsrc.Undef_Valid AND Tsrc.Is_Undefined(V))
    then
        Fin := NaN; 
    else
        Fin := Tsrc.To_Float(V);
    end if;

    Fout := A + B * Fin;

    if(Tdst.Undef_Valid AND Is_Undef_Float(Fout))
    then
        Vout := Tdst.Get_Undefined;
    else
        Vout := Tdst.To_Numeric(Fout);
    end if;



    return Vout;
end Linear;



end Scaling;

