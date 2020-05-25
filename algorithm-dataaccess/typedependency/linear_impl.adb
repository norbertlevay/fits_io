
with Ada.Text_IO;

with Pool_From_String; use Pool_From_String;
with Pool_V3Type_Convs; use Pool_V3Type_Convs;

package body Linear_Impl is

    package TIO renames Ada.Text_IO;


procedure AB_From_Header(HData : in Header.Linear_Conv_Rec; A,B : out Tout)
is
begin
    TIO.Put_Line("Linear_Impl::From_Header(out A B)");
    A := To_V3Type(HData.A);
    B := To_V3Type(HData.B);
end AB_From_Header;


procedure BLANK_From_Header(HData : in Header.Linear_Conv_Rec; BV : out Boolean; Undef : out Tin)
is
begin
    TIO.Put_Line("Linear_Impl::From_Header(out BLANK)");
    BV := HData.BV;
    if(HData.BV)
    then
        Undef := To_V3Type(HData.BLANK);
    end if;

end BLANK_From_Header;



-- used for FF II UU IU UI and IFnoBLANK
function Linear_Pure(Vin : Tin; A,B:Tout) return Tout
is
begin
    TIO.Put_Line("Linear_Impl::Linear_Pure");
    return (A + B * (+Vin));
end Linear_Pure;



-- used for IFwBLANK and FIwBLANKout (BLANKout must exist)
function Linear_Check_UndefIn(Vin : Tin; A,B:Tout) return Tout
is
    Vout : Tout;
begin
    TIO.Put_Line("Linear_Impl::Linear_CheckUndefIn");
    if(Is_Undef(Vin)) then return Do_Undef(Vout);
    else return (A + B * (+Vin));
    end if;
end Linear_Check_UndefIn;



function Is_Undef_BLANK(Vin : Tin) return Boolean
is
begin
    TIO.Put_Line("Linear_Impl::Is_Undef_BLANK");
    return False;
end Is_Undef_BLANK;

function Is_Undef_NaN(Vin : Tin) return Boolean
is
begin
    TIO.Put_Line("Linear_Impl::Is_Undef_NaN");
    return False;
end Is_Undef_NaN;



function Do_Undef_BLANK(Vout : Tout) return Tout
is
begin
    TIO.Put_Line("Linear_Impl::Do_Undef_BLANK");
    return Vout;
end Do_Undef_BLANK;




end Linear_Impl;

