
package body Numeric_Type is

function Bit_Count return Positive
is
V : T := To_Numeric(0.0);
begin
    return V'Size;
end Bit_Count;

function BITPIX return Integer
is
V : T := To_Numeric(0.0);
begin
    return To_BITPIX(V);
end BITPIX;




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



 function To_Numeric(Af : in Float_Arr) return Numeric_Arr
 is
     An : Numeric_Arr(Af'Range);
 begin
    for I in Af'Range
    loop
        An(I) := To_Numeric(Af(I));
    end loop;
    return An;
 end To_Numeric;



 function To_Float(An : in Numeric_Arr) return Float_Arr
 is
     Af : Float_Arr(An'Range);
 begin
    for I in An'Range
    loop
        Af(I) := To_Float(An(I));
    end loop;
    return Af;
 end To_Float;

end Numeric_Type;

