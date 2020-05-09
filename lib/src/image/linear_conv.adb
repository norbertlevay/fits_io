


package body Linear_Conv is


function FF(Vin : Tin) return Tout is begin if(Vin = Vin) then return Tout(A+B*Tc(Vin)); else return ToutNaN; end if; end FF;
function FI(Vin : Tin) return Tout is begin return Tout(A+B*Tc(Vin)); end FI;
function FU(Vin : Tin) return Tout is begin return Tout(A+B*Tc(Vin)); end FU;

function rF(Vin : Tin) return Tout is begin if(Vin = Vin) then return Tout(A+B*Tc(Vin)); else return Tout'First; end if; end rF;-- FIXME Tout'First incorrect, raise excpetion instead
function II(Vin : Tin) return Tout is begin return Tout(A+B*Tc(Vin)); end II;
function IU(Vin : Tin) return Tout is begin return Tout(A+B*Tc(Vin)); end IU;

function UF(Vin : Tin) return Tout is begin if(Vin = Vin) then return Tout(A+B*Tc(Vin)); else return Tout'First; end if; end UF;-- FIXME Tout'First incorrect, raise excpetion instead
function UI(Vin : Tin) return Tout is begin return Tout(A+B*Tc(Vin)); end UI;
function UU(Vin : Tin) return Tout is begin return Tout(A+B*Tc(Vin)); end UU;



-- variants with BLANK: FI FU

function FI_BLANK(Vin : Tin) return Tout is begin if(Vin = BLANKin) then return ToutNaN; else return Tout(A+B*Tc(Vin)); end if; end FI_BLANK;
function FU_BLANK(Vin : Tin) return Tout is begin if(Vin = BLANKin) then return ToutNaN; else return Tout(A+B*Tc(Vin)); end if; end FU_BLANK;


-- variants with BLANK: IF UF

function rF_NaN(Vin : Tin) return Tout
is
    V: Tout;
begin
    if(Vin = Vin)
    then
        V:=Tout(A+B*Tc(Vin));
        if(V=BLANKout)
        then
            null; -- FIXME raise exception "BLANK found among valid values"
        end if;
        return V;
    else
        return BLANKout;
    end if;
end rF_NaN;

function UF_NaN(Vin : Tin) return Tout
is
    V: Tout;
begin
    if(Vin = Vin)
    then
        V:=Tout(A+B*Tc(Vin));
        if(V=BLANKout)
        then
            null; -- FIXME raise exception "BLANK found among valid values"
        end if;
        return V;
    else
        return BLANKout;
    end if;
end UF_NaN;






end Linear_Conv;
