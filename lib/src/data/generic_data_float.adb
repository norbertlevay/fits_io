
-- with Ada.Text_IO; use Ada.Text_IO;

package body Generic_Data_Float is


-- Value conversions

function Physical(BZERO : in T; BSCALE : in T; Data : in T) return T
is
begin
  return BZERO + BSCALE * Data;
end Physical;

end Generic_Data_Float;
