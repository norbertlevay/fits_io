
with Ada.Exceptions; use Ada.Exceptions;

package body Generic_Data_Value is

 
 function Physical_Value(Va : in Tin) return Tout
 is
 begin
  return BZERO + BSCALE * (+Va);
 end Physical_Value;


 function Checked_Physical_Value(Va : in Tin) return Tout
 is
  function PhysVal is new Physical_Value(Tin,Tout,BZERO,BSCALE,"+","*","+");
 begin
  if(BLANK = Va)
  then
    Raise_Exception(Undefined_Value'Identity,"Undefined value encountered");
    null; -- FIXME raise exception Undefined_Value
  end if;
  return PhysVal(Va);
 end Checked_Physical_Value;

end Generic_Data_Value;

