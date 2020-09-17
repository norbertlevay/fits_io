
with Numeric_Type;


generic
with package Tsrc is new Numeric_Type(<>);
with package Tdst is new Numeric_Type(<>);
package Scaling is


function Undefined(V : in Tsrc.Numeric) return Tdst.Numeric;


end Scaling;

