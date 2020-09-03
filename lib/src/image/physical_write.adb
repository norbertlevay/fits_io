
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

package body Physical_Write is

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



  procedure Write_Array
    (F : SIO.File_Type;
    Data        : in Tm_Arr;     -- Data and its
    Undef_Value : in Tm;         -- undef value
    Undef_Valid : in Boolean;    -- exist or not
    A,B        : in Tc;          -- BZERO BSCALE
    Uout_Value : in out Tf;      -- BLANK
    Uout_Valid : in out Boolean) -- BLANK to Header or not
 is
    type Tf_Arr is array (Positive_Count range <>) of Tf;
    RawData : Tf_Arr(Data'First .. Data'Last);
    package Tf_Raw  is new Raw(Tf,Tf_Arr);
    package T_Value is new Value(Tout => Tf, Tc => Tc, Tin => Tm);
begin

    T_Value.A := A;
    T_Value.B := B;
    T_Value.UInValid := Undef_Valid;
    T_Value.UIn      := Undef_Value;

    -- init undef-value

    T_Value.Init_Undef(T_Value.UInValid, T_Value.UIn, UOut_Valid, UOut_Value);

    -- scale array-values

    for I in RawData'Range
    loop
        RawData(I) := T_Value.Scaling(Data(I));
    end loop;

    Tf_Raw.Write_Array(F, RawData);

 end Write_Array;



end Physical_Write;

