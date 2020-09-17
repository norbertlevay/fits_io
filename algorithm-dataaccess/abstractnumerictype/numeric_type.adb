
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


 -- Undefined Value handling

  procedure Set_Undefined(U : in Numeric)
  is
  begin
      Undef := U;
      Undef_Valid := True;
  end Set_Undefined;



  function  Get_Undefined return Numeric
  is
  begin
      return Undef;
  end Get_Undefined;



  function  Is_Undefined(V : in Numeric) return Boolean
  is
  begin
      return Is_Undef(V,Undef);
  end Is_Undefined;




end Numeric_Type;

