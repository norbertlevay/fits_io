
with Ada.Text_IO;

package body Linear_Impl is

    package TIO renames Ada.Text_IO;


    -- Undef values init


 -- F->F  UI->F
 function Init_UOut_Tf2F
     (UinValid : in Boolean; UIn: in Tf;
     UOutValid: in out Boolean; UOut: in out Tm) return Boolean
 is
 begin
     UOut       := UOutNaN;
     UOutValid  := True;
     return False;
 end Init_UOut_Tf2F;
 -- return FnnNaN regardless of UIn



-- F->UI
 function Init_UOut_F2UI
     (UinValid : in Boolean; UIn: in Tf; 
     UOutValid: in out Boolean; UOut: in out Tm) return Boolean
 is
 begin
     return False;
 end Init_UOut_F2UI;
 -- User _must_ supply
 -- do nothing / or check if supplied by user and do Warning if not
 -- if UOut not valid, Read_* will raise except -> Error"UOut must be supplied ba User" otherwise return UOut if encountered Invalid input




-- UI->UI
 function Init_UOut_UI2UI
     (UinValid : in Boolean; UIn: in Tf; 
     UOutValid: in out Boolean; UOut: in out Tm) return Boolean
 is
 begin

    if(UInValid AND NOT UOutValid)
    then
        return True;
        -- BLANK found + user did not supply value -> calculate it by UOut := Scaling(UIn)
    else
        return False;
    end if;

 end Init_UOut_UI2UI;




    -- Scaling









 procedure Check_InValue_Null(Vin,UIn: in Tf; UInValid: Boolean; UOut: in Tm;
     Vout : in out Tm; OutValSet : in out Boolean)
 is begin
--TIO.Put("N");
     null; end Check_InValue_Null;

 procedure Check_OutValue_Null(Vin: in Tf; Vout,UOut: in Tm)
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

 procedure Check_OutValue_F2UI(Vin: in Tf; Vout,UOut: in Tm)
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

