
-- implement for all types:
--
--    PhysVal = BZERO + BSCALE * ArrVal
--
-- handle undefined values (BLANK or NaN): raise exception Undefined_Value

-- useable for these type categories
--
-- digits : Float_nn
-- range  : Integer_nn
-- mod    : Unsigned_nn

package Generic_Data_Value is

 generic
  type Tin  is private;
  type Tout is private;
  BZERO  : in Tout;
  BSCALE : in Tout;
  with function "+" (L, R : in Tout) return Tout is <>;
  with function "*" (L, R : in Tout) return Tout is <>;
  with function "+" (R : in Tin) return Tout is <>;
function Physical_Value(Va : in Tin) return Tout;

 generic
  type Tin  is private;
  type Tout is private;
  BZERO  : in Tout;
  BSCALE : in Tout;
  with function Is_Undefined(Va : in Tin) return Boolean;
  with function "+" (L, R : in Tout) return Tout is <>;
  with function "*" (L, R : in Tout) return Tout is <>;
  with function "+" (R : in Tin) return Tout is <>;
function Checked_Physical_Value(Va : in Tin) return Tout;
-- raise excpetion if Undefined_Value (=BLANK) encountered

Undefined_Value : exception;

-- FIXME replace BLANK : Tin with function Is_Undefined_Value(V : in T) return Boolean
-- or consider digits <>; and range <>; separately -> then all funcs in Data_Unit also duplicated
-- IEEE Floats NaN is not one value but range of values:
-- signbit 0 -> +NaN
-- signbit 0 -> -NaN
-- exponent bits -> all 1
-- fraction bits -> all not 0 <-- this is trouble
--
-- Needs func not constant:
-- Is_Undefined_Value(V : in T)
-- --- for Integers compare to BLANK
-- --- for Floats check:  exponent = 7FFF..  AND fraction /= 0000...

end Generic_Data_Value;

