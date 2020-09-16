

generic
type T is private;
package Numeric_Type is
    type Numeric is new T;
    type T_Arr is array (Positive range <>) of T;
    function Bit_Count return Positive;
end Numeric_Type;

