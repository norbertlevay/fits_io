
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



-- Scaling variant


 procedure Check_InValue_Null(Vin,UIn: in Tf; UInValid: Boolean; UOut: in Tm;
     Vout : in out Tm; OutValSet : in out Boolean)
 is begin
--TIO.Put("N");
     null; end Check_InValue_Null;

 procedure Check_OutValue_Null(Vin,UIn: in Tf; Vout,UOut: in Tm)
 is begin null; end Check_OutValue_Null;


 --    UI -> F
 procedure Check_InValue_BLANK(Vin,UIn: in Tf; UInValid: Boolean; UOut: in Tm;
     Vout : in out Tm; OutValSet : in out Boolean)
 is
 begin
--TIO.Put("DF");
     if(UInValid AND (Vin = UIn)) then OutValSet := True; Vout := UOutNaN; end if;
 end Check_InValue_BLANK;


 --    F -> UI
 procedure Check_InValue_F2UI(Vin,UIn: in Tf; UInValid: Boolean; UOut: in Tm;
     Vout : in out Tm; OutValSet : in out Boolean)
 is
 begin
--TIO.Put("FD");
     if(Vin = Vin) then OutValSet := True; Vout := UOutUser; end if;
 end Check_InValue_F2UI;

 procedure Check_OutValue_F2UI(Vin,UIn: in Tf; Vout,UOut: in Tm)
 is
 begin
--TIO.Put("O");
     -- UOutUser must not be one of valid output values
     if( (Vout = UOutUser) AND (Vin = Vin))
     then
         null; -- raise exception "Vout set invalid however Vin valid value: incorrect UOutUser"
     end if; 
 end Check_OutValue_F2UI;



end Linear_Impl;

