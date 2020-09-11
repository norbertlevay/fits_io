
with Ada.Text_IO;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;
with Interfaces;

with Mandatory;     use Mandatory; -- NAXIS_Arr needed
with Keyword_Record; use Keyword_Record; -- FIndex needed in NAXIS_Arr
with Raw_Funcs; use Raw_Funcs;
with Raw.Data_Unit;
with Header;

with Scaling;

package body DU_Type.Physical.Data_Unit is

  use SIO;

  package TIO renames Ada.Text_IO;

    type Tf_Arr is array (Positive_Count range <>) of Tf;
    package Tf_Raw is new Raw(Tf,Tf_Arr);
    package Tf_Raw_DU is new Tf_Raw.Data_Unit;



 -- Read Write procedures



 procedure Read_Data_Unit
  (File : SIO.File_Type;
  NAXISn : in NAXIS_Arr;
  A,B : in Tc)
 is

    package TT_Scaling is new Scaling(Tm,Tc,Tf);

    procedure RawData(E : in Tf)
    is
        Eout : Tm := TT_Scaling.Linear(E);
    begin
            Data_Elem(Eout);
    end RawData;

    procedure RawData_NoUndefs(E : in Tf)
    is
        Eout : Tm := TT_Scaling.Pure_Linear(E);
    begin
            Data_Elem(Eout);
    end RawData_NoUndefs;

    procedure Read_DU           is new Tf_Raw_DU.Read_Data_Unit_By_Element(RawData);
    procedure Read_DU_NoUndefs  is new Tf_Raw_DU.Read_Data_Unit_By_Element(RawData_NoUndefs);

 begin

    TT_Scaling.A := A;
    TT_Scaling.B := B;

    -- scale array-values

    if(TT_Scaling.Is_Undef_Inited)
    then
        Read_DU(File, NAXISn);
    else
        Read_DU_NoUndefs(File, NAXISn);
    end if;

 end Read_Data_Unit;








 procedure Write_Data_Unit
         (File  : SIO.File_Type;
         NAXISn : in NAXIS_Arr;
         A,B    : in Tc)
 is

    package TT_Scaling is new Scaling(Tf,Tc,Tm);

    procedure RawData(E : out Tf) 
    is
        Em : Tm; 
    begin
        Data_Elem(Em);
        E := TT_Scaling.Linear(Em);
    end RawData;

    procedure RawData_NoUndefs(E : out Tf) 
    is
        Em : Tm; 
    begin
        Data_Elem(Em);
        E := TT_Scaling.Pure_Linear(Em);
    end RawData_NoUndefs;

    procedure Write_DU is
        new Tf_Raw_DU.Write_Data_Unit_By_Element(Tf_DataPadding, RawData);
    procedure Write_DU_NoUndefs is
        new Tf_Raw_DU.Write_Data_Unit_By_Element(Tf_DataPadding, RawData_NoUndefs);

 begin

    TT_Scaling.A := A;
    TT_Scaling.B := B;

    -- scale array-values

    if(TT_Scaling.Is_Undef_Inited)
    then
        Write_DU(File, NAXISn);
    else
        Write_DU_NoUndefs(File, NAXISn);
    end if;

 end Write_Data_Unit;



end DU_Type.Physical.Data_Unit;

