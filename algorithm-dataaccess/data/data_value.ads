
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

package Data_Value is

 -- normal scaling for Ints and Floats with validity check (For floats means Undefined pixels) 
 generic
  type Tin  is private;
  type Tout is private;
  BZERO  : in Tout;
  BSCALE : in Tout;
  Undef_Val : in Tout; 
 with function Is_Valid(V : in Tin) return Boolean is <>;
  with function "+" (L, R : in Tout) return Tout is <>;
  with function "*" (L, R : in Tout) return Tout is <>;
  with function "+" (R : in Tin) return Tout is <>;
 function Valid_Scaled_Value(Va : in Tin) return Tout;
 -- raises exception Invalid_Value
 
 -- BLANK given in header (use for Integers)
 generic
  type Tin  is private;
  type Tout is private;
  BZERO  : in Tout;
  BSCALE : in Tout;
  BLANK  : in Tin;
  Undef_Val : in Tout; 
  with function Is_Valid(V : in Tin) return Boolean is <>;
  with function "+" (L, R : in Tout) return Tout is <>;
  with function "*" (L, R : in Tout) return Tout is <>;
  with function "+" (R : in Tin) return Tout is <>;
 function Matched_Valid_Scaled_Value(Va : in Tin) return Tout;
 -- raises exception Invalid_Value
 -- raises exception Undefined_Value if (=BLANK) encountered

 Invalid_Value   : exception; -- if T'Valib attrib False
 Undefined_Value : exception; -- if match with BLANK


 -- optimized special cases

 -- BZERO BSCALE = 0.0 1.0
 generic
  type Tin  is private;
  type Tout is private;
  Undef_Val : in Tout; 
  with function Is_Valid(V : in Tin) return Boolean is <>;
  with function "+" (R : in Tin) return Tout is <>;
 function Valid_Value(Va : in Tin) return Tout;
 -- raises exception Invalid_Value


 -- BZERO BSCALE = Tab11 (BLANK don't care)
 -- FIXME add BLANK check but also invert the Vin which is BLANK
 generic
  type Tin  is private;--(<>); -- any discrete type
  type Tout is private;--(<>); -- any discrete type
  Undef_Val : in Tout; 
  with function Is_Valid(V : in Tin) return Boolean is <>;
 function Conv_Signed_Unsigned(Vin : in Tin) return Tout;




--private

 -- implements Vout = BZERO + BSCALE * (+Vin)
 generic
  type Tin  is private;
  type Tout is private;
  BZERO  : in Tout;
  BSCALE : in Tout;
  with function "+" (L, R : in Tout) return Tout is <>;
  with function "*" (L, R : in Tout) return Tout is <>;
  with function "+" (R : in Tin) return Tout is <>;
 function Scaled_Value(Va : in Tin) return Tout;




-- new trial

-- integers (BLANK gets also converted - higher level func should 
-- convert and return converted BLANK-value

 generic
    type Tf is private;
    type Tm is private;
    type Tc is digits <>;
    BZERO  : in Tc;
    BSCALE : in Tc;
    with function "+"(R : in Tc) return Tm is <>;
    with function "+"(R : in Tf) return Tc is <>;
 function Scale(Vf : Tf) return Tm;


-- float, check with 'Valid

 generic
    type Tf is digits <>;
    type Tm is private;
    type Tc is digits <>;
    BZERO  : in Tc;
    BSCALE : in Tc;
    Undef  : in Tm; -- returns this when Vf invalid
    with function "+"(R : in Tc) return Tm is <>;
    with function "+"(R : in Tf) return Tc is <>;
 function Scale_Float(Vf : Tf) return Tm;






end Data_Value;

