

package body Value is

 LocUOut      : Tout;
 LocUOutValid : Boolean := False;
 -- holds the values for Scaling()




procedure Init_Undef
    (UInValid : in Boolean;     UIn : in Tin;
    UOutValid : in out Boolean; UOut : in out Tout)
is
    Do_Scaling : Boolean;
begin
    Do_Scaling := Init_UOut(UInValid, UIn, UOutValid, UOut);
    if(Do_Scaling)
    then
        UOut      := +(A + B * (+UIn));
        UOutValid := True;
    end if;

    LocUOut := UOut;
    LocUOutValid := UOutValid;

end Init_Undef;






function Scaling(Vin : Tin) return Tout
is
    Vout : Tout;
begin

    if(Is_Undef(Vin, UIn, UInValid))
    then

        -- convert Undefined value

        if(LocUOutValid)
        then
            Vout := LocUOut;
        else
            null; -- raise exception: "UOut needed but was not given"
        end if;

    else

        -- convert normal value

        Vout := +(A + B * (+Vin));

        -- Vout may be undefined-value only-and-only-if input is undefined-value
        -- verify output is not Undefined value (User may specify UndefOut incorrectly)

        if(Is_Undef(Vout, LocUOut, LocUOutValid))
        then
            null;-- FIXME raise excpetion:
            -- "Incorrect UOut: Vout is Undef but Vin was not"
        end if;

    end if;

    return Vout;
end Scaling;

end Value;
