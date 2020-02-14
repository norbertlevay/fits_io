
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Conversion;

package body Data_Value is

 function Scaled_Value(Va : in Tin) return Tout
 is
 begin
  return BZERO + BSCALE * (+Va);
 end Scaled_Value;


 
 function Valid_Scaled_Value(Va : in Tin) return Tout
 is
  function ScVal is new Scaled_Value(Tin,Tout,BZERO,BSCALE);
 begin
  -- FIXME 'Valid flag behaviour details for Floats: 
  -- are Inf and other special values invalid or only NaN ?
  if(Is_Valid(Va))
  then
  	return ScVal(Va);
  else 
  	return Undef_Val;
  end if; 
 end Valid_Scaled_Value;




 function Matched_Valid_Scaled_Value(Va : in Tin) return Tout
 is
  function ScVal is new Scaled_Value(Tin,Tout,BZERO,BSCALE);
 begin
  if(Is_Valid(Va) AND (Va /= BLANK))
  then
  	return ScVal(Va);
  else
  	return Undef_Val;
  end if;
 end Matched_Valid_Scaled_Value;


 function Valid_Value(Va : in Tin) return Tout
 is
 begin
  if(Is_Valid(Va))
  then
  	return (+Va);
  else 
  	return Undef_Val;
  end if; 
 end Valid_Value;




 function Conv_Signed_Unsigned(Vin : in Tin) return Tout
 is
   type BArr is array (1 .. Tin'Size) of Boolean;
   pragma Pack (BArr);
   function Tin2BArr  is new Ada.Unchecked_Conversion(Tin,BArr);
   function BArr2Tout is new Ada.Unchecked_Conversion(BArr,Tout);
   Arr : BArr := Tin2BArr(Vin);
 begin
  -- FIXME convert sign-unsigned by flipping MSB bit
  Arr(Arr'Last) := not Arr(Arr'Last);
   if(Is_Valid(Vin))
  then
 	return BArr2Tout(Arr);
  else 
  	return Undef_Val;
  end if; 
 end Conv_Signed_Unsigned;

-- function U8_To_I8   is new Conv_Signed_Unsigned(Unsigned_8, Integer_8);
-- function I16_To_U16 is new Conv_Signed_Unsigned(Integer_16, Unsigned_16);
-- function I32_To_U32 is new Conv_Signed_Unsigned(Integer_32, Unsigned_32);
-- function I64_To_U64 is new Conv_Signed_Unsigned(Integer_32, Unsigned_32);




end Data_Value;

