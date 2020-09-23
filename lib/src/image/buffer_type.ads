with Ada.Streams.Stream_IO;use Ada.Streams.Stream_IO;


generic
type T is private;
type Buffer is array (Positive_Count range <>) of T;
--Length : in SIO.Positive_Count := 2880;
A : in out Float;-- := 0.0;
B : in out Float;-- := 1.0;

with function "+"(V : in Float) return T     is <>; 
with function "+"(V : in T)     return Float is <>; 
with function Is_Undef(V,U : in T) return Boolean is <>; 
with function To_BITPIX(V : in T) return Integer is <>; 

package Buffer_Type is

    package SIO renames Ada.Streams.Stream_IO;

procedure Read_Buffer(F: SIO.File_Type; Item : out Buffer);
procedure Write_Buffer(F: SIO.File_Type; Item : in Buffer);

end Buffer_Type;

