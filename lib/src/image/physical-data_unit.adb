
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

procedure Header_Info(Cards : in Optional.Card_Arr; A,B : out Tc;
     BV : out Boolean; BLANK : out Tf)
is
    AStr : String(1..20);-- BZERO
    BStr : String(1..20);-- BSCALE
    UStr : String(1..20);-- BLANK = Undefined value
begin
    TIO.Put_Line("Physical_Read::Header_Info");

    if(Header.Has_Card(Cards, "BZERO   ",AStr))
    --then A := To_V3Type(AStr);
    then A := Tc'Value(AStr);
    --else A := To_V3Type("0.0");
    else A := Tc(0.0);
    end if;

    if(Header.Has_Card(Cards, "BSCALE  ",BStr))
    --then B := To_V3Type(BStr);
    then B := Tc'Value(BStr);
    -- FIXME later implement proper FITS-Float parsing: Tc'Value not always in line with FITS Float syntax
--    else B := To_V3Type("1.0");
    else B := Tc(1.0);
    end if;

    if(Header.Has_Card(Cards, "BLANK   ",UStr))
    then 
        BLANK := To_V3Type(UStr);
        BV    := True;
    else
        BV    := False;
    end if;

    TIO.Put_Line("A B : [" & Tc'Image(A) &", " & Tc'Image(B) & "]");
    if(BV) then TIO.Put_Line("BLANK : " & Ustr); else TIO.Put_Line("BLANK not in Header"); end if;

end Header_Info;



 -- Read Write procedures





 procedure Read_Data_Unit
  (File : SIO.File_Type;
  NAXISn : in NAXIS_Arr;
  Undef_Value : in out Tm;
  Undef_Valid : in out Boolean;
  Cards : in Optional.Card_Arr)
 is

    type Tf_Arr is array (Positive_Count range <>) of Tf;
    package Tf_Raw is new Raw(Tf,Tf_Arr);
    package Tf_Raw_DU is new Tf_Raw.Data_Unit;

    package TT_Scaling is new Scaling(Tm,Tc,Tf);

    procedure RawData(E : in Tf)
    is
        Eout : Tm := TT_Scaling.Linear(E);
    begin
        if(Is_Undef(Eout, Undef_Value, Undef_Valid))
        then
            Undef_Elem(Eout);
        else
            Data_Elem(Eout);
        end if;
    end RawData;

    procedure Read_DU is new Tf_Raw_DU.Read_Data_Unit_By_Element(RawData);

 begin

     Header_Info(Cards, TT_Scaling.A,TT_Scaling.B, TT_Scaling.UInValid, TT_Scaling.UIn);

    -- init undef-value

    TT_Scaling.Init_Undef(TT_Scaling.UInValid, TT_Scaling.UIn, Undef_Valid, Undef_Value);

    -- scale array-values

    Read_DU(File, NAXISn);

 end Read_Data_Unit;








 procedure Write_Data_Unit
         (File : SIO.File_Type;
         NAXISn : in NAXIS_Arr;
         Undef_Value : in out Tm; 
         Undef_Valid : in out Boolean;
         A,B        : in Tc;          -- BZERO BSCALE
         Uout_Value : in out Tf;      -- BLANK
         Uout_Valid : in out Boolean) -- BLANK to Header or not
 is

    type Tf_Arr is array (Positive_Count range <>) of Tf; 
    package Tf_Raw is new Raw(Tf,Tf_Arr);
    package Tf_Raw_DU is new Tf_Raw.Data_Unit;

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
    TT_Scaling.UInValid := Undef_Valid;
    TT_Scaling.UIn      := Undef_Value;

    -- init undef-value

    TT_Scaling.Init_Undef(TT_Scaling.UInValid, TT_Scaling.UIn, UOut_Valid, UOut_Value);

    -- scale array-values

    Write_DU(File, NAXISn);

 end Write_Data_Unit;



end Physical.Data_Unit;

