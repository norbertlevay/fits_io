--
-- Various calculations formulas from [FITS].
--
-- FIXME error/exception handling missing
-- FIXME rename/re-examine FPositive FNatural definition in FITSlib.ads

with Mandatory; -- NAXIS_Arr needed

with Keyword_Record;
use  Keyword_Record;

package File_Funcs is
        
    --
    -- Data array size calculations for all HDU types
    --

    -- implements [FITS] Eq(1)
    
    function PrimaryImage_DataSize_bits
        (BITPIX : Integer;
         NAXIS  : Mandatory.NAXIS_Arr) return FPositive;


    -- implements [FITS] Eq(2)
    
    function ConformingExtension_DataSize_bits
        (BITPIX : Integer;
         NAXIS  : Mandatory.NAXIS_Arr;
         PCOUNT : FNatural;
         GCOUNT : FPositive) return FPositive;


    -- implements [FITS] Eq(4) 

     function RandomGroups_DataSize_bits
        (BITPIX : Integer;
         NAXIS  : Mandatory.NAXIS_Arr;
         PCOUNT : FNatural;
         GCOUNT : FPositive) return FPositive;


end File_Funcs;
