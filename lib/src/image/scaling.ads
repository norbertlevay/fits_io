
with Numeric_Type;

generic
 with package Raw      is new Numeric_Type(<>);
 with package Physical is new Numeric_Type(<>);
package Scaling is

function To_Physical
    (Raw_Arr : in  Raw.Numeric_Arr;
    A,B : in Float)
    return Physical.Numeric_Arr;

function To_Raw
    (Phys_Arr : in Physical.Numeric_Arr;
    A,B : in Float)
    return Raw.Numeric_Arr;

end Scaling;

