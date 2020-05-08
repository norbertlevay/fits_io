

package body Value is

--generic
--type Tout is private;
--type Tin  is private;
--type Tc   is digits <>;
--Undef_Val_In  : Tin;
--Undef_Val_Out : Tout;
--with function Is_Undef(Vin  : Tin ) return Boolean;
--with function Found_Undef(Vin : Tin) return Tout;
--with function Is_Undef(Vout : Tout) return Boolean;
--with function in2c(Vin : Tin) return Tc;
--with function c2out(Vc : Tc) return Tout;
function Linear(A,B : Tc; Vin : Tin) return Tout
is
    Vout : Tout;
begin

if(Is_Undef(Vin)) then return Found_Undef(Vin);
else 
    Vout := c2out( A + B * in2c(Vin));
end if;

if(Is_Undef(Vout)) 
then
    null; -- raisse excpetion "NaN found but no BLANKout provided"
end if;

return Vout;

end Linear;



end Value;


