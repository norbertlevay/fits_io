--
-- Various calculations formulas from [FITS].
--
-- FIXME error/exception handling missing
-- FIXME rename/re-examine FPositive FNatural definition in FITSlib.ads

with Strict; -- Positive_Arr needed

with Keyword_Record;
use  Keyword_Record;

package Formulas is
		
	--
	-- Data array size calculations for all HDU types
	--

	-- implements [FITS] Eq(1)
	
	function PrimaryImage_DataSize_bits
		(BITPIX : Integer;
		 NAXIS  : Strict.Positive_Arr) return FPositive;


	-- implements [FITS] Eq(2)
	
	function ConformingExtension_DataSize_bits
		(BITPIX : Integer;
		 NAXIS  : Strict.Positive_Arr;
		 PCOUNT : FNatural;
		 GCOUNT : FPositive) return FPositive;


	-- implements [FITS] Eq(4) 

	 function RandomGroups_DataSize_bits
		(BITPIX : Integer;
		 NAXIS  : Strict.Positive_Arr;
		 PCOUNT : FNatural;
		 GCOUNT : FPositive) return FPositive;


end Formulas;
