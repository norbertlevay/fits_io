



package body Linear_Private is



function Linear(Vin : in Tin) return Tout
is
begin
    if(Is_Undef(Vin))
    then
        return Handle_Undef(Vin);
    else
        return +( A + B * (+Vin) );
    end if;
end Linear;





end Linear_Private;

