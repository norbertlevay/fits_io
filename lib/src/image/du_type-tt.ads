
 
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Mandatory; use Mandatory; -- NAXIS_Arr needed
with Optional; -- Card_Arr needed

with DU_Type.Physical;
with DU_Type.Physical.Data_Unit;

generic
    with function Init_UOut(UInValid : in Boolean; UIn : in Tm; 
            UOutValid : in out Boolean; UOut : in out Tm) return Boolean is <>; 

    with function Is_Undef(V,U : Tm; UValid : Boolean) return Boolean is <>; 
    with function "+"(R : Tc) return Tm is <>; 
    with function "+"(R : Tm) return Tc is <>; 
 with function To_V3Type(S : String) return Tm is <>; 

package DU_Type.TT is

    package TTPhysical is new Physical(Tm);
    package TTPhysical_DU is new TTPhysical.Data_Unit;

end DU_Type.TT;

