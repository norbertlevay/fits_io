

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




 generic
 type T is private;
 function Is_Undef_Floats(V,U: T; UValid : Boolean) return Boolean;

 generic
 type T is private;
 function Is_Undef_Ints(V,U: T; UValid : Boolean) return Boolean;

end Linear_Impl;

