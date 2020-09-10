
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

package body Physical.Data_Unit is

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
--        if(Is_Undef(Eout, Undef_Value, Undef_Valid))
--        then
--            Undef_Elem(Eout);
--        else
            Data_Elem(Eout);
--        end if;
    end RawData;

    procedure Read_DU is new Tf_Raw_DU.Read_Data_Unit_By_Element(RawData);

 begin

    TT_Scaling.A := A;
    TT_Scaling.B := B;

    -- scale array-values

    Read_DU(File, NAXISn);

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

    procedure Write_DU is new Tf_Raw_DU.Write_Data_Unit_By_Element(Tf_DataPadding,RawData);

 begin

    TT_Scaling.A := A;
    TT_Scaling.B := B;

    -- scale array-values

    Write_DU(File, NAXISn);

 end Write_Data_Unit;



end Physical.Data_Unit;

