


package body Linear_Conv is

-- NOTES
    -- Vin = Vin is check for NaN: for NaN results of False
-- FIXME
    -- how to return NaN for a given instantiation ? --> we need _generic_NaN_ which gets resolved to F32NaN F64NaN ... at instantiation.
    -- how many actual NaN's exist? IEEE float standard defines 2 F64NaN F32NaN how about 128bit NaN ???
    -- e.g. whichever float-type FITSlib wll support, such float-type must have defined NaN-value
    -- -> example put NaN in V3_Types and user includes V3_Types at instatiation -> NaN gets resolved
    -- ONE SOLUTION: put NaN as generic parameter with enabled default and compiler will puul it in from Vn_Types

function FF(A,B:Tc; Vin : Tin) return Tout is begin if(Vin = Vin) then return Tout(A+B*Tc(Vin)); else return Tout(0.0); end if; end FF;--FIXME Tout(0.0) should be Tout(NaN)
function FI(A,B:Tc; Vin : Tin) return Tout is begin return Tout(A+B*Tc(Vin)); end FI;
function FU(A,B:Tc; Vin : Tin) return Tout is begin return Tout(A+B*Tc(Vin)); end FU;

function rF(A,B:Tc; Vin : Tin) return Tout is begin return Tout(A+B*Tc(Vin)); end rF;

-- FIXME make better names


end Linear_Conv;
