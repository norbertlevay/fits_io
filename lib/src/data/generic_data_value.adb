


package body Generic_Data_Value is

 -- gen for any type
 
 function Physical_Value(Va : in T) return T
 is
 begin
  -- FIXME here check for NaN -> if found raise exception Undefined_Value (like BLANK)
  return BZERO + BSCALE * Va;
 end Physical_Value;



 -- for each type category

 function Physical_Value_From_Float(Va : in TF) return TF
 is
 begin
  -- FIXME here check for NaN -> if found raise exception Undefined_Value (like BLANK)
  return BZERO + BSCALE * Va;
 end Physical_Value_From_Float;




 function Physical_Value_From_Int(Va : in TI) return TF
 is
  function PhysVal is new Physical_Value_From_Float(TF,BZERO, BSCALE);
 begin
  return PhysVal(TF(Va));-- NOTE explicit numeric conversion
 end Physical_Value_From_Int;



 function Physical_Value_From_Int_With_BLANK(Va : in TI) return TF
 is
  function PhysVal is new Physical_Value_From_Int(TI,TF,BZERO, BSCALE);
 begin
  if(BLANK = Va)
  then
    null;-- FIXME raise exception -> good for MinMax: 
		-- skip undefed values, they do not participate in MinMax search
  end if;
  return PhysVal(Va);
 end Physical_Value_From_Int_With_BLANK;


 function Physical_Value_From_UInt(Va : in TM) return TF
 is
  function PhysVal is new Physical_Value_From_Float(TF,BZERO, BSCALE);
 begin
  return PhysVal(TF(Va));-- NOTE explicit numeric conversion
 end Physical_Value_From_UInt;



 function Physical_Value_From_UInt_With_BLANK(Va : in TM) return TF
 is
  function PhysVal is new Physical_Value_From_UInt(TM,TF,BZERO, BSCALE);
 begin
  if(BLANK = Va)
  then
    null;-- FIXME raise exception -> good for MinMax: 
		-- skip undefed values, they do not participate in MinMax search
  end if;
  return PhysVal(Va);
 end Physical_Value_From_UInt_With_BLANK;







end Generic_Data_Value;
