
-- implement for all types:
--
--    PhysVal = BZERO + BSCALE * ArrVal
--
-- handle undefined values (BLANK or NaN): raise exception Undefined_Value

-- use type categories
--
-- digits : Float_nn
-- range  : Integer_nn
-- mod    : Unsigned_nn

package Generic_Data_Value is

 -- any type

 generic
  type T is private;
  BZERO  : in out T;
  BSCALE : in out T;
  with function "*" (L, R : in T) return T;
  with function "+" (L, R : in T) return T;
function Physical_Value(Va : in T) return T;


 -- explict for each type-categora 

 generic
  type TF is digits <>;
  BZERO  : in out TF;
  BSCALE : in out TF;
function Physical_Value_From_Float(Va : in TF) return TF;


 generic
  type TI is range <>; -- FIXME how abount Unsigned_8 ?  
  type TF is digits <>;
  BZERO  : in out TF;
  BSCALE : in out TF;
function Physical_Value_From_Int(Va : in TI) return TF;
-- use when BLANK not available

 generic
  type TI is range <>; -- FIXME how abount Unsigned_8 ?  
  type TF is digits <>;
  BZERO  : in out TF;
  BSCALE : in out TF;
  BLANK  : in out TI;
function Physical_Value_From_Int_With_BLANK(Va : in TI) return TF;
-- use when BLANK available (Integers only)

 generic
  type TM is mod <>; -- FIXME how abount Unsigned_8 ?  
  type TF is digits <>;
  BZERO  : in out TF;
  BSCALE : in out TF;
function Physical_Value_From_UInt(Va : in TM) return TF;
-- use when BLANK not available

 generic
  type TM is mod <>; -- FIXME how abount Unsigned_8 ?  
  type TF is digits <>;
  BZERO  : in out TF;
  BSCALE : in out TF;
  BLANK  : in out TM;
function Physical_Value_From_UInt_With_BLANK(Va : in TM) return TF;
-- use when BLANK available (Integers only)


end Generic_Data_Value;




