
with Ada.Text_IO;

package body Scaling_Impl is

    package TIO renames Ada.Text_IO;


    -- Undef values init


 -- F->F  UI->F
    -- substitute: use F.NaN (Ignore UOut/BLANK)
 function Init_UOut_Tf2F
     (UinValid : in Boolean; UIn: in Tf;
     UOutValid: in out Boolean; UOut: in out Tm) return Boolean
 is
 begin
--    TIO.Put_Line("Value_Impl::Init_UOut_Tf2F");
     UOut       := UOutNaN;
     UOutValid  := True;
     return False;
 end Init_UOut_Tf2F;
 -- return FnnNaN regardless of UIn



-- F->UI
 -- substitute: use User supplied value
 function Init_UOut_F2UI
     (UinValid : in Boolean; UIn: in Tf; 
     UOutValid: in out Boolean; UOut: in out Tm) return Boolean
 is
 begin
--    TIO.Put_Line("Value_Impl::Init_UOut_F2UI");
     return False;
 end Init_UOut_F2UI;
 -- User _must_ supply
 -- do nothing / or check if supplied by user and do Warning if not
 -- do not raise ERROR because: 
 -- if not supplied by user and the FITS-file data contains no Undef values, data read will be ok
 -- if UOut not valid, Read_* will raise except -> Error"UOut must be supplied ba User" otherwise return UOut if encountered Invalid input




-- UI->UI
 -- use User supplied value; if not given, calc by Scaling(UIn)
 function Init_UOut_UI2UI
     (UinValid : in Boolean; UIn: in Tf; 
     UOutValid: in out Boolean; UOut: in out Tm) return Boolean
 is
 begin
--    TIO.Put_Line("Value_Impl::Init_UOut_UI2UI");

    if(UInValid AND NOT UOutValid)
    then
        return True;
        -- BLANK found + user did not supply value -> calculate it by UOut := Scaling(UIn)
    else
        return False;
    end if;

 end Init_UOut_UI2UI;




 function Is_Undef_Floats(V,U: T; UValid : Boolean) return Boolean
 is
 begin
     return Not (V = V);
 end Is_Undef_Floats;

 function Is_Undef_Ints(V,U: T; UValid : Boolean) return Boolean
 is
 begin
     if(UValid)
     then
         return (V = U);
     else
         return False;
     end if;
 end Is_Undef_Ints;

end Scaling_Impl;

