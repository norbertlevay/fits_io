
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
with Raw;
with Header;

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
    TIO.Put_Line("Linear_Impl::From_Header");

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
        BLANK := To_V3Type(AStr);
        BV    := True;
    else
        BV    := False;
    end if;

end Header_Info;



-- Scaling

 procedure Check_InValue(Vin,UIn: in Tf; UOut: in Tm; 
     Vout : in out Tm; OutValSet : in out Boolean)
 is begin null; end Check_InValue;

 procedure Check_OutValue(Vin,UIn: in Tf; Vout,UOut: in Tm)
 is begin null; end Check_OutValue;

 Uin :Tf;
 Uout:Tm;
 A,B : Tc;

 -- variant no BLANK
function Scaling (Vin : in Tf; A,B : Tc) return Tm
 is
     Vout : Tm;
 begin

     Vout := +(A + B * (+Vin));

     Check_OutValue(Vin,UIn,Vout,UOut);-- FIXME needs Dummy val for UIn !?

     return Vout;
 end Scaling;





 -- variant with BLANK
 function Scaling (Vin : in Tf; A,B : Tc; BLANK : Tf) return Tm
 is
     Vout : Tm;
     VoutSet : Boolean := False;
 begin

     Check_InValue(Vin,UIn,UOut, Vout,VoutSet);

     if(not VoutSet) then Vout := +(A + B * (+Vin)); end if;
     -- FIXME relies on optimization for NullFunc case:
     -- ValSet remains False and so if(no ValSet).. = if(not False)..
     -- would optimization remove the if-check (evaluate during compile time after instantiation ??
     Check_OutValue(Vin,UIn,Vout,UOut);

     return Vout;
 end Scaling;









 -- Read Write procedures

  procedure Read_Array
    (F : SIO.File_Type;
    Data  : out Tm_Arr;
    Cards : in Optional.Card_Arr)
  is
    type Tf_Arr is array (Positive_Count range <>) of Tf;
    RawData : Tf_Arr(Data'First .. Data'Last);
    package Tf_Raw is new Raw(Tf,Tf_Arr);
     -- info from Header:
    A,B   : Tc;
    BV    : Boolean;
    BLANK : Tf;
 begin
    Header_Info(Cards, A,B, BV, BLANK);
    Tf_Raw.Read_Array(F, RawData);
    for I in RawData'Range
    loop
      Data(I) := Linear(RawData(I), A,B, BV, BLANK);
    end loop;
  end Read_Array;





 procedure Read_All
  (File : SIO.File_Type;
  NAXISn : in NAXIS_Arr;
  Cards : in Optional.Card_Arr)
 is
     -- info from Header:
    A,B   : Tc;
    BV    : Boolean;
    BLANK : Tf;

    type Tf_Arr is array (Positive_Count range <>) of Tf;
    package Tf_Raw is new Raw(Tf,Tf_Arr);

    procedure RawData(E : in Tf)
    is
    begin
            Data_Elem(Linear(E, A,B, BV, BLANK));
    end RawData;

    procedure Read_DU is new Tf_Raw.Read_All(RawData);

 begin
    Header_Info(Cards, A,B, BV, BLANK);
    Read_DU(File, NAXISn);
 end Read_All;







  procedure Read_Volume
    (File : SIO.File_Type;
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    Last    : in NAXIS_Arr;
    Volume  : out Tm_Arr;
    Cards : in Optional.Card_Arr)
  is
    VolLength : Positive_Count := Raw_Funcs.Volume_Length(First, Last);

    type Tf_Arr is array (Positive_Count range <>) of Tf;
    RawVol: Tf_Arr(1 .. VolLength);

    package Tf_Raw is new Raw(Tf,Tf_Arr);

    -- info from Header:
    A,B   : Tc;
    BV    : Boolean;
    BLANK : Tf;
  begin
    Header_Info(Cards, A,B, BV, BLANK);

    Tf_Raw.Read_Volume(File, DUStart, NAXISn, First, Last, RawVol);

    for I in RawVol'Range
    loop
      Volume(I) := Linear(RawVol(I), A,B, BV, BLANK);
    end loop;

  end Read_Volume;


end Physical_Read;

