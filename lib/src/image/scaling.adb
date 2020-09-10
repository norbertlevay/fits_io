

package body Scaling is

 LocUOut      : Tout;
 LocUOutValid : Boolean := False;
 -- holds the values for Scaling()

 UIn      : Tin;
 UInValid : Boolean := False;



procedure Init_Undef
    (UInValid : in Boolean;     UIn : in Tin;
    UOutValid : in out Boolean; UOut : in out Tout)
is
    Do_Linear : Boolean;
begin
    Scaling.UIn      := UIn;
    Scaling.UInValid := UInValid;

    Do_Linear := Init_UOut(Scaling.UInValid, Scaling.UIn, UOutValid, UOut);
    if(Do_Linear)
    then
        UOut      := Linear(UIn);
        UOutValid := True;
    end if;

    LocUOut := UOut;
    LocUOutValid := UOutValid;

end Init_Undef;






function Linear(Vin : Tin) return Tout
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
end Linear;

end Scaling;
