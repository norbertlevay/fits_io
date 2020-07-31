

package Linear_Impl is


    -- Undef values init


 -- F->F  UI->F
 generic
 type Tf is private;
 type Tm is private;
 UOutNaN : in Tm;
 function Init_UOut_Tf2F
     (UinValid : in Boolean; UIn: in Tf;
     UOutValid: in out Boolean; UOut: in out Tm) return Boolean;
 -- return FnnNaN regardless of UIn
 -- indicate Scaling not needed



-- F->UI
 generic
 type Tf is private;
 type Tm is private;
 function Init_UOut_F2UI
     (UinValid : in Boolean; UIn: in Tf;
     UOutValid: in out Boolean; UOut: in out Tm) return Boolean;
 -- indicate Scaling(UIn) not needed
 -- ignore UIn, check for NaN:
 -- if UOut not valid -> Error"UOut must be supplied ba User" otherwise return UOut




-- UI->UI
 generic
 type Tf is private;
 type Tm is private;
 function Init_UOut_UI2UI
     (UinValid : in Boolean; UIn: in Tf;
     UOutValid: in out Boolean; UOut: in out Tm) return Boolean;
 -- indicate UOut:=Scaling(UIn) needed, if UIn valid and UOut invalid




    -- Scaling


-- Impl1: with BLANK: FF UU II UI IU
-- (Check_InValue is not called when no BLANK)

 generic
 type Tf is private;
 type Tm is private;
 procedure Check_InValue_Null(Vin,UIn: in Tf;  UInValid: Boolean; UOut: in Tm;
     Vout : in out Tm; OutValSet : in out Boolean);

 generic
 type Tf is private;
 type Tm is private;
 procedure Check_OutValue_Null(Vin: in Tf; Vout,UOut: in Tm);


-- Impl2: used only UI-> F, with BLANK
-- (Check_InValue is not called when no BLANK)

 generic
 type Tf is private;
 type Tm is private;
 UOutNaN : in Tm;
 procedure Check_InValue_BLANK(Vin,UIn: in Tf; UInValid: Boolean; UOut: in Tm;
     Vout : in out Tm; OutValSet : in out Boolean);


-- Impl3: used only F -> UI
-- (ignore BLANK)

 generic
 type Tf is private;
 type Tm is private;
 procedure Check_InValue_F2UI(Vin,UIn: in Tf; UInValid: Boolean;  UOut: in Tm;
     Vout : in out Tm; OutValSet : in out Boolean);

 generic
 type Tf is private;
 type Tm is private;
 procedure Check_OutValue_F2UI(Vin: in Tf; Vout,UOut: in Tm);



end Linear_Impl;

