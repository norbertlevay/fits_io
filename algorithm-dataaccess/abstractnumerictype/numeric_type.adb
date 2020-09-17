
package body Numeric_Type is

function Bit_Count return Positive
is
V : T;
begin
    return V'Size;
end Bit_Count;


  -- conversions to/from Float

  function To_Numeric(V : in Float) return T
  is
  begin
      return +V;
  end To_Numeric;

  function To_Float  (V : in T) return Float
  is
  begin
      return +V;
  end To_Float;


end Numeric_Type;

