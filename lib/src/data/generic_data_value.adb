
with Ada.Exceptions; use Ada.Exceptions;

with Ada.Unchecked_Conversion;-- NOT IN USE only for refrence

package body Generic_Data_Value is

 
 function Physical_Value(Va : in Tin) return Tout
 is
 begin
  return BZERO + BSCALE * (+Va);
 end Physical_Value;




 function Checked_Physical_Integer(Va : in Tin) return Tout
 is
  function PhysVal is new Physical_Value(Tin,Tout,BZERO,BSCALE,"+","*","+");
 begin
  if(Va = BLANK)
  then
    Raise_Exception(Undefined_Value'Identity,"Undefined value encountered");
  end if;
  return PhysVal(Va);
 end Checked_Physical_Integer;





 -- undefined float values use IEEE NaN : 
 -- signbit=don't care & 
 -- exponent= all ones & 
 -- fraction= any but not all zeros
 generic
  type T is digits <>; 
 function Is_NaN(V : in T) return Boolean;
 function Is_NaN(V : in T) return Boolean
 is
 begin
  return ( (T'Exponent(V) = 255) AND 
           (T'Fraction(V) /= 0.0) );
 end Is_NaN;



 function Checked_Physical_Float(Va : in Tin) return Tout
 is
  function "+" (Vin : in Tin) return Tout is begin return Tout(Vin); end "+";
  function FloatPhysVal is new Physical_Value(Tin, Tout, BZERO, BSCALE);
  function IsNaN   is new Is_NaN(Tin);
 begin
  if(IsNaN(Va))
  then
    Raise_Exception(Undefined_Value'Identity,"Undefined value encountered");
  end if;
  return FloatPhysVal(Va);
 end Checked_Physical_Float;



------------------------------------------------------------
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

