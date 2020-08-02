

package body Value is

 LocUOut      : Tm;
 LocUOutValid : Boolean := False;
 -- holds the values for Scaling()




procedure Init_Undef
    (UInValid : in Boolean;     UIn : in Tf;
    UOutValid : in out Boolean; UOut : in out Tm)
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






function Scaling(Vin : Tf) return Tm
is
    Vout : Tm;
begin

    if(Is_Undef(Vin, UIn, UInValid))
    then

        if(LocUOutValid)
        then
            Vout := LocUOut;
        else
            null; -- raise exception: "UOut needed but was not given"
        end if;

    else

        Vout := +(A + B * (+Vin));

        if(Is_Undef(Vout, LocUOut, LocUOutValid))
        then
            null;-- FIXME raise excpetion:
            -- "Incorrect UOut: Vout is Undef but Vin was not or vica-versa"
        end if;

    end if;

    return Vout;
end Scaling;

end Value;
