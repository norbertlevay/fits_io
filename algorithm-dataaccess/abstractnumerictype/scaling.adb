



package body Scaling is



procedure Set_Undefined(U : in Tsrc.Numeric)
is
begin
    Tsrc.Set_Undefined(U);
    Tdst.Set_Undefined(Linear(U));
end Set_Undefined;






function Is_Undef_Float(F : in Float) return Boolean
is
begin
    return (Not (F = F));
end Is_Undef_Float;
-- NOTE one possibility to generate NaN's: good?
--   Zero : Float := 0.0; -- FIXME from ieee.adb - said to be NON-PORTABLE ??? define NaN values
--   NaN  : Float := 0.0/Zero;-- FIXME  define NaN values for various Float-types
 





function Linear(V : in Tsrc.Numeric) return Tdst.Numeric
is
    Vout : Tdst.Numeric;
    Fin  : Float;
    Fout : Float;
    Use_Undefs : Boolean := (Tsrc.Undef_Valid AND Tdst.Undef_Valid);
begin

    -- instead of Use_Undefs-flag, could use OOP/tagged-records and dispatching ->
    -- would save to repeat the else branch; however would be an overkill:
    -- there are only two cases: we have or have-not Undef values

    if(Use_Undefs)
    then

        -- data has Undefs

        if(Tsrc.Is_Undefined(V))
        then
            Vout := Tdst.Get_Undefined;
        else

            Fin  := Tsrc.To_Float(V);
            Fout := A + B * Fin;
            Vout := Tdst.To_Numeric(Fout);

            if(Tdst.Is_Undefined(Vout))
            then
                null;-- error: "Vout undefined but Vin was not."
            end if;

        end if;

    else

        -- data has no Undefs

        Fin  := Tsrc.To_Float(V);
        Fout := A + B * Fin;
        Vout := Tdst.To_Numeric(Fout);

    end if;

    return Vout;

end Linear;



end Scaling;

