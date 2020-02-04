
with Ada.Exceptions; use Ada.Exceptions;

with Ada.Unchecked_Conversion;-- NOT IN USE only for refrence

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
  if(Is_Undefined(Va))
  then
    Raise_Exception(Undefined_Value'Identity,"Undefined value encountered");
  end if;
  return PhysVal(Va);
 end Checked_Physical_Value;


-- optimized version for sign conversions (flip highest bit)
-- NOT IN USE, is only for reference

 generic
  type Tin  is (<>); -- any discrete type
  type Tout is (<>); -- any discrete type
 function Conv_Signed_Unsigned(Vin : in Tin) return Tout;

 function Conv_Signed_Unsigned(Vin : in Tin) return Tout
 is
   type BArr is array (1 .. Tin'Size) of Boolean;
   pragma Pack (BArr);
   function Tin2BArr  is new Ada.Unchecked_Conversion(Tin,BArr);
   function BArr2Tout is new Ada.Unchecked_Conversion(BArr,Tout);
   Arr : BArr := Tin2BArr(Vin);
 begin
  -- convert sign-unsigned by flipping MSB bit
  Arr(Arr'Last) := not Arr(Arr'Last);
  return BArr2Tout(Arr);
 end Conv_Signed_Unsigned;

-- function U8_To_I8   is new Conv_Signed_Unsigned(Unsigned_8, Integer_8);
-- function I16_To_U16 is new Conv_Signed_Unsigned(Integer_16, Unsigned_16);
-- function I32_To_U32 is new Conv_Signed_Unsigned(Integer_32, Unsigned_32);
-- function I64_To_U64 is new Conv_Signed_Unsigned(Integer_32, Unsigned_32);




end Generic_Data_Value;

