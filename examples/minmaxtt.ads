

with Numeric_Type;
with Array_IO;

generic
 type T is private;
 T_Last : T;
 T_First : T;
 with function ">"(L,R : T) return Boolean is <>;
 with function "<"(L,R : T) return Boolean is <>;
package MinmaxTT is

Min : T := T_Last;
Max : T := T_First;

procedure MinMax(V : in T);

function To_String(V : T) return String;

end MinmaxTT;
