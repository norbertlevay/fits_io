
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Mandatory; use Mandatory; -- NAXIS_Arr needed
with Optional; -- Card_Arr needed


generic
package Physical.Data_Unit is

 package SIO renames Ada.Streams.Stream_IO;





     generic
     with procedure Data_Elem (Elem : in Tm);
     with procedure Undef_Elem(Elem : in Tm);
     with function Init_UOut(UInValid : in Boolean;
        UIn : in Tf; UOutValid : in out Boolean; UOut : in out Tm) return Boolean is <>;
     with function Is_Undef(V,U : Tf; UValid : Boolean) return Boolean is <>;
     with function Is_Undef(V,U : Tm; UValid : Boolean) return Boolean is <>;
     with function "+"(R : Tf) return Tc is <>;
     with function "+"(R : Tc) return Tm is <>; 
     procedure Read_Data_Unit
         (File : SIO.File_Type;
         NAXISn : in NAXIS_Arr;
         Undef_Value : in out Tm;
         Undef_Valid : in out Boolean;
         A,B : in Tc;
         UIn_Value : in Tf;
         UIn_Valid : in Boolean);
         --Cards : Optional.Card_Arr);





    generic
     Tf_DataPadding : Tf;
     with procedure Data_Elem (Elem : out Tm);
     with function Init_UOut(UInValid : in Boolean; UIn : in Tm; 
          UOutValid : in out Boolean; UOut : in out Tf) return Boolean is <>; 
     with function Is_Undef(V,U : Tm; UValid : Boolean) return Boolean is <>; 
     with function Is_Undef(V,U : Tf; UValid : Boolean) return Boolean is <>; 
     with function "+"(R : Tm) return Tc is <>; 
     with function "+"(R : Tc) return Tf is <>; 
     procedure Write_Data_Unit
         (File : SIO.File_Type;
         NAXISn : in NAXIS_Arr;
         Undef_Value : in out Tm; 
         Undef_Valid : in out Boolean;
         A,B        : in Tc;           -- BZERO BSCALE
         Uout_Value : in out Tf;       -- BLANK
         Uout_Valid : in out Boolean); -- BLANK to Header or not



end Physical.Data_Unit;

