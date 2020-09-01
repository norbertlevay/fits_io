
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Mandatory; use Mandatory; -- NAXIS_Arr needed
with Optional; -- Card_Arr needed


generic
  type Tm is private;   -- type in memory
  type Tm_Arr is array (Positive_Count range <>) of Tm;
  type Tc is digits <>; -- type in which scaling is calculated
  type Tf is private;   -- type in fits-file

with function Init_UOut(UInValid : in Boolean; UIn : in Tf; UOutValid : in out Boolean; UOut : in out Tm) return Boolean is <>;

with function Is_Undef(V,U : Tf; UValid : Boolean) return Boolean is <>;
with function Is_Undef(V,U : Tm; UValid : Boolean) return Boolean is <>;

with function "+"(R : Tf) return Tc is <>;
with function "+"(R : Tc) return Tm is <>;
--with function To_V3Type(Arg : String) return Tc is <>;
with function To_V3Type(Arg : String) return Tf is <>;
with function To_V3Type(Arg : String) return Tm is <>;


package Physical_Write is

 package SIO renames Ada.Streams.Stream_IO;



     procedure Write_Array
         (F : SIO.File_Type;
         Data : in Tm_Arr;
         Undef_Value : in out Tm;
         Undef_Valid : in out Boolean;
         Cards : out Optional.Card_Arr);



end Physical_Write;

