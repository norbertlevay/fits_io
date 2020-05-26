
with Ada.Text_IO;

with Pool_From_String; use Pool_From_String;
with Pool_V3Type_Convs; use Pool_V3Type_Convs;

package body Conv_Impl is

    package TIO renames Ada.Text_IO;


-- used for FF II UU IU UI and IFnoBLANK
function Conv_Pure(Vin : Tin; BV:Boolean; UndefIn : Tin; UndefOut : Tout ) return Tout
is
begin
    TIO.Put_Line("Conv_Impl::Conv_Pure");
    return (+Vin);
end Conv_Pure;







-- used for IFwBLANK
function Conv_Check_UndefIn(Vin : Tin; BV:Boolean; UndefIn : Tin; UndefOut : Tout ) return Tout
is
    Vout : Tout;
begin
    TIO.Put_Line("Conv_Impl::Conv_CheckUndefIn");
    if(BV AND Is_Undef(Vin)) then return Do_Undef(Vout);
    else return (+Vin);
    end if;
end Conv_Check_UndefIn;







-- used for FIwBLANKout (BLANKout must exist)
function Conv_Check_UndefOut(Vin : Tin; BV:Boolean; UndefIn : Tin; UndefOut : Tout) return Tout
is
    Vout : Tout;
begin
    TIO.Put_Line("Conv_Impl::Conv_CheckUndefOut");
    if(BV AND Is_Undef(Vin)) then return Do_Undef(UndefOut);
    else 
        Vout := (+Vin);
        if(Vout = UndefOut) then null; end if;--raise exception:'UndefOut returned by valid input'
        return Vout;
    end if;
end Conv_Check_UndefOut;




function Is_Undef_BLANK(Vin : Tin) return Boolean
is
begin
    TIO.Put_Line("Conv_Impl::Is_Undef_BLANK");
    return False;
end Is_Undef_BLANK;

function Is_Undef_NaN(Vin : Tin) return Boolean
is
begin
    TIO.Put_Line("Conv_Impl::Is_Undef_NaN");
    return False;
end Is_Undef_NaN;



function Do_Undef_BLANK(Vout : Tout) return Tout
is
begin
    TIO.Put_Line("Conv_Impl::Do_Undef_BLANK");
    return Vout;
end Do_Undef_BLANK;




end Conv_Impl;

