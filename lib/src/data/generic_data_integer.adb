
--with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

package body Generic_Data_Integer is


 function Min(B : in Data.Block; B_Min : in T) return T
 is
  Min : T := B_Min;
 begin
  for I in Data.Block'Range
  loop
    if ( B(I) < Min)
    then
      Min := B(I);
    end if;
  end loop;
  return Min;
 end Min;

 function Max(B : in Data.Block; B_Max : in T) return T
 is
  Max : T := B_Max;
 begin
  for I in Data.Block'Range
  loop
    if ( B(I) < Max)
    then
      Max := B(I);
    end if;
  end loop;
  return Max;
 end Max;







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
