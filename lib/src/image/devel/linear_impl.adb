
with Ada.Text_IO;

--with Pool_From_String; use Pool_From_String;
with Pool_String_To_V3types; use Pool_String_To_V3types;
with Pool_V3Type_Convs; use Pool_V3Type_Convs;

with Optional; -- Card_Arr needed
with Header;-- Has_Card() needed

package body Linear_Impl is

    package TIO renames Ada.Text_IO;




--generic
--type Tin  is private;
--type Tc   is digits <>;
--type Tout is private;
procedure From_Header(Cards : in Optional.Card_Arr; A,B : out Tout;
     BV : out Boolean; BLANK : out Tin)
is
    AStr : String(1..20);-- BZERO
    BStr : String(1..20);-- BSCALE
    UStr : String(1..20);-- BLANK = Undefined value
begin
    TIO.Put_Line("Linear_Impl::From_Header");

    if(Header.Has_Card(Cards, "BZERO   ",AStr))
    then A := To_V3Type(AStr);
    else A := To_V3Type("0.0");
    --else A := Tc(0.0);
    end if;

    if(Header.Has_Card(Cards, "BSCALE  ",BStr))
    then B := To_V3Type(BStr);
    else B := To_V3Type("1.0");
    --else B := Tc(1.0);
    end if;

    if(Header.Has_Card(Cards, "BLANK   ",UStr))
    then 
        BLANK := To_V3Type(AStr);
        BV    := True;
    else
        BV    := False;
    end if;

end From_Header;





--generic
--type Tin  is private;
--type Tout is private;
function Linear_Pure(Vin : Tin; A,B:Tout; BV: Boolean; BLANK : Tin) return Tout
is
--    Vout : Tout;
begin
    TIO.Put_Line("Linear_Impl::Linear_Pure");
    return (A + B * (+Vin));
end Linear_Pure;


--generic
--type Tin  is private;
--type Tout is private;
function Linear_Check_UndefIn(Vin : Tin; A,B:Tout; BV: Boolean; BLANK : Tin) return Tout
is
    Vout : Tout;
begin
    TIO.Put_Line("Linear_Impl::Linear_CheckUndefIn");
    return Vout;
end Linear_Check_UndefIn;


end Linear_Impl;

