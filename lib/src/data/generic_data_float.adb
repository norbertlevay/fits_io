
-- with Ada.Text_IO; use Ada.Text_IO;

package body Generic_Data_Float is


-- Value conversions

function Physical_Value(BZERO : in T; BSCALE : in T; Array_Value : in T) return T
is
begin
  return BZERO + BSCALE * Array_Value;
end Physical_Value;

end Generic_Data_Float;
