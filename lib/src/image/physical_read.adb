
-- Sequential Access:
-- if T_Arr'Length = NAXISi*NAXISi-1*...*NAXIS1
-- then repeating Read (NAXISn*NAXISn-1*...NAXISi+1)-times 
-- reads all DU sequentially
-- NOTE position to DUStart before 1st call in Read/Write_x_Data


-- NOTE we separate to Int and Float because Scale_Float checks 
-- that value is valid and Scale used fpr Int does not check validity.
-- Because in case of (U)Int, BLANK is in range of valid values
-- whereas for Floats NaN is invalid in Ada, woudl raise exception
-- for (U)Int:  new-BLANK = A + B * BLANK
-- for Float:   A + B * NaN -> raises exception

with Ada.Text_IO;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;
with Interfaces;

with Mandatory;     use Mandatory; -- NAXIS_Arr needed
with Keyword_Record; use Keyword_Record; -- FIndex needed in NAXIS_Arr
with Raw_Funcs; use Raw_Funcs;
with Raw.Data_Unit;
with Header;

with Value_Impl;
with Value;

package body Physical_Read is

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



  procedure Read_Array
    (F : SIO.File_Type;
    Data  : out Tm_Arr;
    Undef_Value : in out Tm;
    Undef_Valid : in out Boolean;
    Cards : in Optional.Card_Arr)
  is
    type Tf_Arr is array (Positive_Count range <>) of Tf;
    RawData : Tf_Arr(Data'First .. Data'Last);
    package Tf_Raw is new Raw(Tf,Tf_Arr);
    package T_Value is new Value(Tm,Tc,Tf);
  begin

    Header_Info(Cards, T_Value.A,T_Value.B, T_Value.UInValid, T_Value.UIn);

    -- init undef-value

    T_Value.Init_Undef(T_Value.UInValid, T_Value.UIn, Undef_Valid, Undef_Value);

    -- scale array-values

    Tf_Raw.Read_Array(F, RawData);

    for I in RawData'Range
    loop
       Data(I) := T_Value.Scaling(RawData(I));
    end loop;

 end Read_Array;





  procedure Read_Volume
    (File : SIO.File_Type;
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    Last    : in NAXIS_Arr;
    Volume  : out Tm_Arr;
    Undef_Value : in out Tm;
    Undef_Valid : in out Boolean;
    Cards : in Optional.Card_Arr)
  is
    VolLength : Positive_Count := Raw_Funcs.Volume_Length(First, Last);

    type Tf_Arr is array (Positive_Count range <>) of Tf;
    RawVol: Tf_Arr(1 .. VolLength);

    package Tf_Raw is new Raw(Tf,Tf_Arr);
    package T_Value is new Value(Tm,Tc,Tf);

  begin
    Header_Info(Cards, T_Value.A,T_Value.B, T_Value.UInValid, T_Value.UIn);

    -- init undef-value

    T_Value.Init_Undef(T_Value.UInValid, T_Value.UIn, Undef_Valid, Undef_Value);

    -- scale array-values

    Tf_Raw.Read_Volume(File, DUStart, NAXISn, First, Last, RawVol);

    for I in RawVol'Range
    loop
      Volume(I) := T_Value.Scaling(RawVol(I));
    end loop;

  end Read_Volume;





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

    package T_Value is new Value(Tm,Tc,Tf);

    procedure RawData(E : in Tf)
    is
        Eout : Tm := T_Value.Scaling(E);
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

     Header_Info(Cards, T_Value.A,T_Value.B, T_Value.UInValid, T_Value.UIn);

    -- init undef-value

    T_Value.Init_Undef(T_Value.UInValid, T_Value.UIn, Undef_Valid, Undef_Value);

    -- scale array-values

    Read_DU(File, NAXISn);

 end Read_Data_Unit;


end Physical_Read;

