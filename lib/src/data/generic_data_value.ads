
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
 function Scaled_Value(Va : in Tin) return Tout;
 
 generic
  type Tin  is private;
  type Tout is private;
  BZERO  : in Tout;
  BSCALE : in Tout;
  with function Is_Valid(V : in Tin) return Boolean is <>;
  with function "+" (L, R : in Tout) return Tout is <>;
  with function "*" (L, R : in Tout) return Tout is <>;
  with function "+" (R : in Tin) return Tout is <>;
 function Valid_Scaled_Value(Va : in Tin) return Tout;
 -- raises exception Invalid_Value

 generic
  type Tin  is private;
  type Tout is private;
  BZERO  : in Tout;
  BSCALE : in Tout;
  BLANK  : in Tin;
  with function Is_Valid(V : in Tin) return Boolean is <>;
  with function "+" (L, R : in Tout) return Tout is <>;
  with function "*" (L, R : in Tout) return Tout is <>;
  with function "+" (R : in Tin) return Tout is <>;
 function Checked_Valid_Scaled_Value(Va : in Tin) return Tout;
 -- raises exception Invalid_Value
 -- raises exception Undefined_Value if (=BLANK) encountered


 Invalid_Value   : exception; -- if T'Valib attrib False
 Undefined_Value : exception; -- if match with BLANK



 generic
  type Tin  is private;--(<>); -- any discrete type
  type Tout is private;--(<>); -- any discrete type
 function Conv_Signed_Unsigned(Vin : in Tin) return Tout;

end Generic_Data_Value;

