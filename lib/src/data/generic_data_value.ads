


package Generic_Data_Value is
 

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




