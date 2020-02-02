
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
  BZERO  : in out Tout;
  BSCALE : in out Tout;
  with function "+" (L, R : in Tout) return Tout;
  with function "*" (L, R : in Tout) return Tout;
  with function "+" (R : in Tin) return Tout;
function Physical_Value(Va : in Tin) return Tout;

 generic
  type Tin  is private;
  type Tout is private;
  BZERO  : in out Tout;
  BSCALE : in out Tout;
  BLANK  : in out Tin;
  with function "+" (L, R : in Tout) return Tout;
  with function "*" (L, R : in Tout) return Tout;
  with function "+" (R : in Tin) return Tout;
function Checked_Physical_Value(Va : in Tin) return Tout;
-- raise excpetion if Undefined_Value (=BLANK) encountered

Undefined_Value : exception;

end Generic_Data_Value;

