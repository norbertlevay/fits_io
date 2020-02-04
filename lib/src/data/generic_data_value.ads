
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
-- FIXME should provide Position where undef val is instead of Va
  with function "+" (L, R : in Tout) return Tout is <>;
  with function "*" (L, R : in Tout) return Tout is <>;
  with function "+" (R : in Tin) return Tout is <>;
function Checked_Physical_Value(Va : in Tin) return Tout;
-- raises excpetion if Undefined_Value encountered

 generic
  type Tin  is private;
  type Tout is private;
  BZERO  : in Tout;
  BSCALE : in Tout;
  BLANK  : in Tin;
  with function "+" (L, R : in Tout) return Tout is <>;
  with function "*" (L, R : in Tout) return Tout is <>;
  with function "+" (R : in Tin) return Tout is <>;
function Checked_Physical_Integer(Va : in Tin) return Tout;
-- raises excpetion if Undefined_Value (=BLANK) encountered

 generic
  type Tin  is digits <>;
  type Tout is digits <>;
  BZERO  : in Tout;
  BSCALE : in Tout;
  with function "+" (L, R : in Tout) return Tout is <>;
  with function "*" (L, R : in Tout) return Tout is <>;
  with function "+" (R : in Tin) return Tout is <>;
function Checked_Physical_Float(Va : in Tin) return Tout;
-- raises excpetion if Undefined_Value (=NaN) encountered



Undefined_Value : exception;


end Generic_Data_Value;

