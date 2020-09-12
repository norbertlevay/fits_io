

package body Scaling is

    -- FIXME these are dummy - remove them
    -- DU_Type::Is_Undef will check against values stored in DU_Type, not here
 LocUOut      : Tout;
 LocUOutValid : Boolean := False;
 UIn      : Tin;
 UInValid : Boolean := False;



function Pure_Linear(Vin : Tin) return Tout
is
begin
    return +(A + B * (+Vin));
end Pure_Linear;



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

--        Vout := +(A + B * (+Vin));
        Vout := Pure_Linear(Vin);

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
