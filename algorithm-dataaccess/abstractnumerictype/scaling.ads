
-- NOTE that type Tcalc is digits <>; can be added to Scaling generic-params
-- so user may decide/fine-tune  which Float-precision to use for Scaling
-- FIXME for now keep Ada.Float for simplicity


with Numeric_Type;

generic

 with package Tsrc is new Numeric_Type(<>);
 with package Tdst is new Numeric_Type(<>);

 with function Is_Undef(V,U : in Tsrc.Numeric) return Boolean is <>;
 with function Is_Undef(V,U : in Tdst.Numeric) return Boolean is <>;

package Scaling is

    A : Float := 0.0;
    B : Float := 1.0;


 type Tsrc_Numeric_Arr is array (Positive range <>) of Tsrc.Numeric;
 type Tdst_Numeric_Arr is array (Positive range <>) of Tdst.Numeric;


procedure Set_Undefined(Us : in Tsrc.Numeric; Ud : in Tdst.Numeric);
-- converts and sets undefined value U to both Tsrc and Tdst

function  Linear(V   : in Tsrc.Numeric) return Tdst.Numeric;
procedure Linear(Ain : in Tsrc_Numeric_Arr; Aout : out Tdst_Numeric_Arr);
-- perform scaling applying Undef-value if defined

end Scaling;

