


-- NOTE we separate to Int and Float because Scale_Float checks 
-- that value is valid and Scale used fpr Int does not check validity.
-- Because in case of (U)Int, BLANK is in range of valid values
-- whereas for Floats NaN is invalid in Ada, woudl raise exception
-- for (U)Int:  new-BLANK = A + B * BLANK
-- for Float:   A + B * NaN -> raises exception

with Ada.Text_IO;


package body Physical_Funcs is


  package TIO renames Ada.Text_IO;


-- Vout = A + B * Vin 

 function Scale(Vf : in Tf) return Tm
 is
 begin
    return +( BZERO + BSCALE * (+Vf) );
 end Scale;

-- NOTE See also 'Machine_Overflows True/False in conjuction with
 -- floating point behaviour  ->  whether NaN Inf will be generated as
 -- result of float operations or some exception raised
 -- GNAT switches like -mieee and -gnato?? affect exception (and also some pragmas?)

 -- How to test for NaN:
 -- example from Rosetta Code (And IEEE-Standard says) shows: NaN = NaN is FALSE,
 -- but PInf = Pinf is TRUE as for any other number

 -- unverified from iNet:
 -- Catching NaN requires a three-way test, viz.:
 -- if Value /= 0.0 and then (not (Value < 0.0)) and then (not (Value > 0.0))
 -- then
 --   <Value is NaN>;
 -- end if;

 function Scale_Float(Vf : in Tf) return Tm
 is
 begin
 --if(Vf'Valid) -- evaluates false besides NaN also for +/-Inf
-- pragma Optimize(Off); -- turn off optional optimizations
   if(Vf = Vf) -- true for any number except NaN NOTE make sure compiler does not optimize it out
 then
    return +( BZERO + BSCALE * Tc(Vf) );
 else
    return Undef;-- retun NaN in Tm type
 end if;
 end Scale_Float;


end Physical_Funcs;

