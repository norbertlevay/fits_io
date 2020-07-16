
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
--type Tout is private;
function Linear_Pure(Vin : Tin; A,B:Tcalc; BV: Boolean; BLANK : Tin) return Tout
is
--    Vout : Tout;
begin
    TIO.Put_Line("Linear_Impl::Linear_Pure");
    return +(A + B * (+Vin));
end Linear_Pure;


--generic
--type Tin  is private;
--type Tout is private;
function Linear_Check_UndefIn(Vin : Tin; A,B:Tcalc; BV: Boolean; BLANK : Tin) return Tout
is
    Vout : Tout;
begin
    TIO.Put_Line("Linear_Impl::Linear_CheckUndefIn");
    return Vout;
end Linear_Check_UndefIn;


end Linear_Impl;

