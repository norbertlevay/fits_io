

package Linear_Impl is

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
 procedure Check_OutValue_Null(Vin,UIn: in Tf; Vout,UOut: in Tm);


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
 UOutUser : in Tm;
 procedure Check_InValue_F2UI(Vin,UIn: in Tf; UInValid: Boolean;  UOut: in Tm;
     Vout : in out Tm; OutValSet : in out Boolean);

 generic
 type Tf is private;
 type Tm is private;
 UOutUser : in Tm;
 procedure Check_OutValue_F2UI(Vin,UIn: in Tf; Vout,UOut: in Tm);



end Linear_Impl;

