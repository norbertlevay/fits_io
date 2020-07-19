
--with Ada.Text_IO;

package body Linear_Impl is

--    package TIO renames Ada.Text_IO;


function Linear_Pure(Vin : Tin; A,B:Tcalc; BV: Boolean; BLANK : Tin) return Tout
is
begin
    return +(A + B * (+Vin));
end Linear_Pure;



-- FIXME separate implementation needed depending on presence/absence of BLANK in header
function Linear_Check_UndefIn(Vin : Tin; A,B:Tcalc; BV: Boolean; BLANK : Tin) return Tout
is
    Vout : Tout;
begin
    if(BV and (Vin = BLANK))
    then
        return UndefOut;
    else
        return +(A + B * (+Vin));
    end if;
end Linear_Check_UndefIn;


end Linear_Impl;

