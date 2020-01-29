
-- with Ada.Text_IO; use Ada.Text_IO;

package body Generic_Data_Float is

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
    if ( B(I) > Max)
    then
      Max := B(I);
    end if;
  end loop;
  return Max;
 end Max;


-- Value conversions

function Physical_Value(BZERO : in T; BSCALE : in T; Array_Value : in T) return T
is
begin
  return BZERO + BSCALE * Array_Value;
end Physical_Value;

end Generic_Data_Float;
