
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Data_Value; use Data_Value;

package body Unit is

procedure Read_Array_Values
   (F : SIO.File_Type;
    Length : in Positive_Count;
    First  : in Positive := 1)
is
begin
    null;
end Read_Array_Values;

procedure Write_Array_Values
   (F : SIO.File_Type;
    Length : in Positive_Count;
    First  : in Positive := 1)
is
begin
    null;
end Write_Array_Values;



 function Scale(Vf : in Tf) return Tm
 is
 begin
    return +( BZERO + BSCALE * (+Vf) );
 end Scale;





 function Scale_Float(Vf : in Tf) return Tm
 is
 begin
 if(Vf'Valid)
 then
    return Undef;
 else
    return +( BZERO + BSCALE * (+Vf) );
 end if;
 end Scale_Float;



end Unit;

