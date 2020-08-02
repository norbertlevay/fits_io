
with Ada.Text_IO;

package body Value_Impl is

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

end Value_Impl;

