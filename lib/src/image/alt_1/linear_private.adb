



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





function Linear_From_Floats(Vin : in Tin) return Tout
is
begin
    if(Vin = Vin) -- False is NaN
    then
        return +( A + B * (+Vin) );
    else
        if(not Undef_Valid)
        then
            null;
            -- raise exception
        end if;
        return Undef_Val_Out;
    end if;
end Linear_From_Floats;


function Linear_From_Ints(Vin : in Tin) return Tout
is
begin
    if(Undef_Valid AND (Vin = Undef_Val_In))
    then
        return Undef_Val_Out;-- can be NaN if Out is Float
    else
        return +( A + B * (+Vin) );
    end if;
end Linear_From_Ints;







end Linear_Private;

