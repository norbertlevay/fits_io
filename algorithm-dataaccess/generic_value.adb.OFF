
with Ada.Unchecked_Conversion;
with Interfaces;

with FITS; -- Byte-type needed

-- DBG only with Ada.Text_IO; use Ada.Text_IO;


package body Generic_Value is

function Physical_From_Int
	(BZERO : in TF; 
	BSCALE : in TF; 
	BLANK : in TD;
	Data  : in TD) return TF
is
  D : TF := TF(Data);
begin
--  if (Data = BLANK) return NaN; end if;
--  FIXME See IEEE 748 standard what bit-pattern is NaN and explicitely use that
  return BZERO + BSCALE * D; 
end Physical_From_Int;


function Physical_From_Float(BZERO : in TF; BSCALE : in TF; Data : in TF) return TF
is
begin
  return BZERO + BSCALE * Data;
end Physical_From_Float;

end Generic_Value;
