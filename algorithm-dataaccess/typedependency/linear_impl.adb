
with Ada.Text_IO;

with Pool_From_String; use Pool_From_String;
with Pool_V3Type_Convs; use Pool_V3Type_Convs;

package body Linear_Impl is

    package TIO renames Ada.Text_IO;




--generic
--type Tin  is private;
--type Tout is private;
procedure From_Header(HData : in Header.Linear_Conv_Rec; A,B : out Tout;
     BV : out Boolean; UndefIn : out Tin; UndefOut : out Tout)
is
begin
    TIO.Put_Line("Linear_Impl::From_Header");
    A := To_V3Type(HData.A);
    B := To_V3Type(HData.B);
    BV := HData.BV;
    if(HData.BV)
    then
        UndefIn  := To_V3Type(HData.BLANK);
      null;
    end if;

end From_Header;





--generic
--type Tin  is private;
--type Tout is private;
function Linear_Pure(Vin : Tin; A,B:Tout) return Tout
is
--    Vout : Tout;
begin
    TIO.Put_Line("Linear_Impl::Linear_Pure");
    return (A + B * (+Vin));
end Linear_Pure;


--generic
--type Tin  is private;
--type Tout is private;
function Linear_Check_UndefIn(Vin : Tin; A,B:Tout) return Tout
is
    Vout : Tout;
begin
    TIO.Put_Line("Linear_Impl::Linear_CheckUndefIn");
    return Vout;
end Linear_Check_UndefIn;


end Linear_Impl;

