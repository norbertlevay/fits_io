
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Numeric_Type;
with Array_IO;

generic
 type T is private;
 type T_Arr is array (Positive_Count range <>) of T;
 type Float_Arr is array (Positive_Count range <>) of Float;
 T_Last : T;
 T_First : T;
 with function ">"(L,R : T) return Boolean is <>;
 with function "<"(L,R : T) return Boolean is <>;

-- from Numeric_Type pool
 with function "+"(V : in T)     return Float is <>;
 with function "+"(V : in Float) return T is <>;

 with function Is_Undef(V,U : in T) return Boolean is <>;
 with function To_BITPIX(V : in T) return Integer is <>;
package MinmaxTT is

Min : T := T_Last;
Max : T := T_First;

procedure MinMax(V : in T);

function To_String(V : T) return String;

end MinmaxTT;
