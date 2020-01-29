
--with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

package body Generic_Data_Integer is

-- Value conversions

function Physical
        (BZERO : in TF; 
        BSCALE : in TF; 
        BLANK : in T; 
        Data  : in T) return TF
is
  D : TF := TF(Data);
begin

  if (Data = BLANK)
  then
	Raise_Exception(Undefined_Value'Identity, "BLANK encountered");
  end if;
 
  return BZERO + BSCALE * D;  

end Physical;


end Generic_Data_Integer;
