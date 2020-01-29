
--with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

package body Generic_Data_Integer is

-- Value conversions

function Physical_Value
        (BZERO : in TF; 
        BSCALE : in TF; 
        BLANK : in T; 
        Array_Value  : in T) return TF
is
  D : TF := TF(Array_Value);
begin

  if (Array_Value = BLANK)
  then
	Raise_Exception(Undefined_Value'Identity, "BLANK encountered");
  end if;
 
  return BZERO + BSCALE * D;  

end Physical_Value;
 -- NOTE: alternative BLANK handling return NaN
 --   F32_NaN : constant Float_32 := Float_32(16#FFFFFFFF#);
 --   F64_NaN : constant Float_64 := Float_64(16#FFFFFFFFFFFFFFFF#);
 -- In Ada prefered is excpetion, NaN can still be returned in exception handler

end Generic_Data_Integer;
