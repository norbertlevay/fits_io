
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

function Undefined(V : in Tsrc.Numeric) return Tdst.Numeric;


end Scaling;

