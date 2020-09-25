with Ada.Streams.Stream_IO;use Ada.Streams.Stream_IO;

-- FITS allows/envcourages to store the data in different type as it is available:
-- [A,B, Target_BITPIX]-triple determines the type in which data is stored and proper scaling
-- so it can be scaled back at Read with:
--  Vout = BZERO + BSCALE * Vin
--  where
--  BSCALE = f(A,B)  BZERO=f(A,B) 
--  So Vout = A + B * Vin used in Write
--  is the inverse func to: Vout = BZERO + BSCALE * Vin

-- Target_BITPIX := 0 -> means store in the same type as T
-- FIXME what to use for Target_Type/DU_Type/File_Type/Target_BITPIX ? 
-- enum or ?


generic
type T is private;
Undefined_Value : T;
Undefined_Valid : Boolean := False;
Target_Undefined_Value : Float   := 0.0; -- FIXME will be converted to target type
Target_Undefined_Valid : Boolean := False; -- if valid
A : in Float := 0.0;
B : in Float := 1.0;
Target_BITPIX : Integer := 0; -- zero means the same as T, no scaling needed A,B=(0,1)

with function "+"(V : in Float) return T     is <>; 
with function "+"(V : in T)     return Float is <>; 
with function Is_Undef(V,U : in T) return Boolean is <>; 
with function To_BITPIX(V : in T) return Integer is <>; 

package Buffer_Type is

    package SIO renames Ada.Streams.Stream_IO;

type Buffer is array (Positive_Count range <>) of T;

procedure Read_Buffer(S: not null access Ada.Streams.Root_Stream_Type'Class; Item : out Buffer);
procedure Write_Buffer(S: not null access Ada.Streams.Root_Stream_Type'Class; Item : in Buffer);

for Buffer'Write use Write_Buffer;
for Buffer'Read use Read_Buffer;
-- normally 'Write 'Read would be overriden on type T not T_Arr
-- but because we have Array_IO due to efficiency (dont ask Undef_Valid and Is_Undef in
-- each cycle-round). 
-- So overriden 'Write 'Read calling (Scaling and Conversion) would be attached to T not T_Arr


end Buffer_Type;

