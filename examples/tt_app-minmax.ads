
with Ada.Streams.Stream_IO;

with Physical.Data_Unit;

  generic
    package TT_App.Minmax is

    package SIO renames Ada.Streams.Stream_IO;
    use type SIO.Count;
    
    Special_Count : SIO.Count := 0; -- Inf...
    Undef_Count   : SIO.Count := 0; -- NaN

    package  T_Physical is new Physical(Tm,Tm_Arr,Tc, Tf);
    package  T_Physical_DU is new T_Physical.Data_Unit;

    procedure Plane_Data(E : Tm);
    procedure Undef_Data(E : Tm);
    procedure Read_Data_Unit is new T_Physical_DU.Read_Data_Unit(Plane_Data);--,Undef_Data);

    procedure Put_Results(UndefValid : in Boolean; UndefValue : in String);

  end TT_App.Minmax;


