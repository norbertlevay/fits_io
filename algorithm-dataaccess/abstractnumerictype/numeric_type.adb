
package body Numeric_Type is

function Bit_Count return Positive
is
V : T;
begin
    return V'Size;
end Bit_Count;

end Numeric_Type;
