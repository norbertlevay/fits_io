
with Numeric_Type;
with Array_IO;

--generic
--type T is private;
--Length : in SIO.Positive_Count := 2880;
--type Buffer is array (SIO.Positive_Count range 1 .. Length) of T;
--A : in out Float := 0.0;
--B : in out Float := 1.0;
package body Buffer_Type is

 type Float_Arr is array (SIO.Positive_Count range <>) of Float;
 type SI_Arr is array (SIO.Positive_Count range <>) of Short_Integer;

 function "+"(V : in Float) return Short_Integer is begin return Short_Integer(V); end "+";
 function "+"(V : in Short_Integer)     return Float is begin return Float(V); end "+";

 function Is_Undef(V,U : in Short_Integer) return Boolean is begin return (V=U); end Is_Undef; 
 function To_BITPIX(V : in Short_Integer) return Integer is begin return V'Size; end To_BITPIX; 


procedure Read_Buffer(F: SIO.File_Type; Item : out Buffer)
is
 package Phys is new Numeric_Type(T, Buffer, Float_Arr);
 package Raw  is new Numeric_Type(Short_Integer, SI_Arr, Float_Arr);
 -- FIXME Raw info normaly would come from Header like A,B; here we simplify: Tf=Tm
 package AIO  is new Array_IO(Raw,Phys);
begin
    AIO.Read(F, A,B, Item);
end Read_Buffer;



procedure Write_Buffer(F: SIO.File_Type; Item : in Buffer)
is
 package Phys is new Numeric_Type(T, Buffer, Float_Arr);
 --package Raw  is new Numeric_Type(T, Buffer, Float_Arr);
 package Raw  is new Numeric_Type(Short_Integer, SI_Arr, Float_Arr);
 -- FIXME normaly some algorithm would decide abuout Type for Raw; here we simplify: Tf=Tm
 package AIO  is new Array_IO(Raw,Phys);

begin
    AIO.Write(F, A,B, Item);
end Write_Buffer;



end Buffer_Type;

