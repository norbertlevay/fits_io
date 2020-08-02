

package body Value is

-- UIn  : Tf;-- = BLANK
-- UInValid  : Boolean := False;
 UOut : Tm;-- FIXME should come from API/user in F->UI cases
 UOutValid : Boolean := False;
-- A,B  : Tc;


procedure Init_Undef
    (UInValid : in Boolean; UIn : in Tf;
    UOutValid : in out Boolean; UOut : in out Tm)
is
    Do_Scaling : Boolean;
begin
    Do_Scaling := Init_UOut(UInValid, UIn, UOutValid, UOut);
    if(Do_Scaling)
    then
        Uout      := +(A + B * (+UIn));
        UOutValid := True;
    end if;
end Init_Undef;









function Scaling(Vin : Tf) return Tm
is
    Vout : Tm;
begin

    if(Is_Undef(Vin, UIn, UInValid))
    then

        if(UOutValid)
        then
            Vout := UOut;
        else
            null; -- raise exception: "UOut needed but was not given"
        end if;

    else

        Vout := +(A + B * (+Vin));

        if(Is_Undef(Vout, UOut, UOutValid))
        then
            null;-- FIXME raise excpetion:
            -- "Incorrect UOut: Vout is Undef but Vin was not or vica-versa"
        end if;

    end if;

    return Vout;
end Scaling;

end Value;
