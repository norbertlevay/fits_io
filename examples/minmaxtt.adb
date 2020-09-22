

with Numeric_Type;
with Array_IO;

package body MinmaxTT is

procedure MinMax(V : in T)
is
begin
    if(V < Min) then Min := V; end if;
    if(V > Max) then Max := V; end if;
end MinMax;


function To_String(V : T) return String
is
begin
    return "Dummy";--T_Image(V);
end To_String;

end MinmaxTT;
