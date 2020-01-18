
with Ada.Unchecked_Conversion;
with Interfaces;
with System; use System;

-- DBG only with Ada.Text_IO; use Ada.Text_IO;

with FITS; -- Byte-type needed


package body Generic_Data_Float is


-- Value conversions

function Physical(BZERO : in T; BSCALE : in T; Data : in T) return T
is
begin
  return BZERO + BSCALE * Data;
end Physical;

end Generic_Data_Float;
