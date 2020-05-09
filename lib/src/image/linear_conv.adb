


package body Linear_Conv is

-- FIXME divide to Input Output so that Tin Tout are swaped:
    -- In  Tin=Tf  Tout=Tm
    -- Out Tin=Tm  Tout=Tf

-- NOTE Vin = Vin is check for NaN: for NaN result is False

-- FIXME
    -- cases which involve Float have to deal with NaN as mark for undefined values
    -- FF -> simple: if(NaN) -> Tout(NaN) and Vn_Types will procide NaN defienitions

    -- FI FU & no   BLANK from Header -> simply scale, no check
    -- FI FU & with BLANK from Header -> if(BLANK) -> Tout(NaN)

    -- IF UF -> what BLANK to use if NaN encoutered in Vin ? -> Ask user to provide BLANK (he must know the data-range, should be able to provide BLANK
    -- IF UF -> no   BLANK -> user did not give BLANK, but NaN found -> raise exception "Data contains undefined values (NaN) but no BLANK given"
    -- IF UF -> with BLANK -> if(NaN) -> return BLANK & check that no value after conversion results in BLANK: if(Vout=BLANK) raise exception "BLANK among valid values"

function FF4R(Vin : Tin) return Tout is begin if(Vin = Vin) then return Tout(A+B*Tc(Vin)); else return ToutNaN; end if; end FF4R;
function FF4W(Vin : Tout) return Tin is begin if(Vin = Vin) then return Tin(A+B*Tc(Vin)); else return TinNaN; end if; end FF4W;

function FI4R(Vin : Tin) return Tout is begin return Tout(A+B*Tc(Vin)); end FI4R;
function FI4W(Vin : Tout) return Tin is begin return Tin(A+B*Tc(Vin)); end FI4W;


function FU(Vin : Tin) return Tout is begin return Tout(A+B*Tc(Vin)); end FU;
function FI_BLANK(Vin : Tin) return Tout is begin if(Vin = BLANK) then return ToutNaN; else return Tout(A+B*Tc(Vin)); end if; end FI_BLANK;
function FU_BLANK(Vin : Tin) return Tout is begin if(Vin = BLANK) then return ToutNaN; else return Tout(A+B*Tc(Vin)); end if; end FU_BLANK;




function rF(Vin : Tin) return Tout
is
begin
    if(Vin = Vin)
    then
        return Tout(A+B*Tc(Vin));
    else
        return Tout'First; -- FIXME raise Exception "UndefVal found but no BLANK provided"
    end if; 
end rF;




    function rF_NaN(Vin : Tin) return Tout
    is 
        V: Tout;
    begin 
        if(Vin = Vin)
        then 
            V:=Tout(A+B*Tc(Vin));
            if(V=BLANK)
            then
                null; -- FIXME raise exception "BLANK found among valid values"
            end if;
            return V;
        else 
            return BLANK;
        end if;
    end rF_NaN;






end Linear_Conv;
