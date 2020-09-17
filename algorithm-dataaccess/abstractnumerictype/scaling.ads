
with Numeric_Type;



-- NOTE that type Tcalc is digits <>; can be added to Scaling generic-params
-- so user may decide/fine-tune  which Float-precision to use for Scaling
-- FIXME for now keep Ada.Float for simplicity

generic
with package Tsrc is new Numeric_Type(<>);
with package Tdst is new Numeric_Type(<>);
package Scaling is

    A : Float := 0.0;
    B : Float := 1.0;

procedure Set_Undefined(U: in Tsrc.Numeric);
-- converts and sets undefined value U to both Tsrc and Tdst

function Linear(V : in Tsrc.Numeric) return Tdst.Numeric;
-- performs scaling applying Undef-value if defined

end Scaling;

