
with Ada.Streams.Stream_IO;

with DU_Type.Physical;
with DU_Type.Physical.Data_Unit;
with V3_Pool_Scaling; use V3_Pool_Scaling;

with Mandatory; use Mandatory; -- NAXIS_Arr needed


  generic
with    package T_Physical    is new DU_Type.Physical(<>);--(Tf);
--with    package T_Physical_DU is new T_Physical.Data_Unit;



 with function T_Valid(V: Tm) return Boolean is <>; 
 with function T_First return Tm is <>; 
 with function T_Last  return Tm is <>; 
 with function T_Image(V: Tm) return String is <>; 
 with function ">"(L,R : Tm)  return Boolean is <>; 
 with function "<"(L,R : Tm)  return Boolean is <>; 








  package DU_Type.Minmax is

    package SIO renames Ada.Streams.Stream_IO;
    use type SIO.Count;

    Special_Count : SIO.Count := 0; -- Inf...
    Undef_Count   : SIO.Count := 0; -- NaN

    procedure Plane_Data(E : in Tm);-- finds min max values
    procedure Read_Data_Unit is new T_Physical.Read_Data_Unit(Plane_Data);
    --procedure Read_Data_Unit is new T_Physical_DU.Read_Data_Unit(Plane_Data);

    procedure Put_Results(UndefValid : in Boolean; UndefValue : in String);




  end DU_Type.Minmax;


