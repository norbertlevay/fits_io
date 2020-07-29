
with Ada.Text_IO;

package body Linear_Impl is

    package TIO renames Ada.Text_IO;


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
     if(Not (Vin = Vin)) then OutValSet := True; Vout := UOut; end if;
 end Check_InValue_F2UI;

 procedure Check_OutValue_F2UI(Vin,UIn: in Tf; Vout,UOut: in Tm)
 is
 begin
--TIO.Put("O");
     -- UOut must not be one of valid output values
     if( (Vout = UOut) AND (Vin = Vin))
     then
         TIO.Put_Line("ERROR: Vout=Undef but Vin not Undef");
         -- raise exception "Vout set invalid however Vin valid value: incorrect UOutUser"
     end if; 
 end Check_OutValue_F2UI;



end Linear_Impl;

