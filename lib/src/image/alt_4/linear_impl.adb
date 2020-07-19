
--with Ada.Text_IO;

package body Linear_Impl is

--    package TIO renames Ada.Text_IO;


function Linear_Pure(Vin : Tin; A,B:Tcalc; BV: Boolean; BLANK : Tin) return Tout
is
begin
--TIO.Put("P");
    return +(A + B * (+Vin));
end Linear_Pure;




-- FIXME separate implementation needed depending on presence/absence of BLANK in header

function Linear_Check_UndefIn(Vin : Tin; A,B:Tcalc; BV: Boolean; BLANK : Tin) return Tout
is
begin
--TIO.Put("I");
    if(BV and (Vin = BLANK))
    then
        return UndefOut;
    else
        return +(A + B * (+Vin));
    end if;
end Linear_Check_UndefIn;





function Linear_Check_UndefOut(Vin : Tin; A,B:Tcalc; BV: Boolean; BLANK : Tin) return Tout
is
    Vout : Tout;
begin
--TIO.Put("O");
    if(Vin = Vin) -- false if  Vin=NaN
    then
        Vout := +(A + B * (+Vin));
        if(Vout = UndefOut)
        then 
            null;-- raise exception: Input not UndefIn but output = UndefOut
        end if;
        return Vout;
    else
        return UndefOut;
    end if;
end Linear_Check_UndefOut;



end Linear_Impl;

