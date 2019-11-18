--
-- Various calculations formulas from [FITS].
--
-- FIXME error/exception handling missing
-- FIXME rename/re-examine FPositive FNatural definition in FITSlib.ads

with Strict; -- Positive_Arr needed

package Formulas is
		
	--
	-- Data array size calculations for all HDU types
	--

	-- implements [FITS] Eq(1)
	
	function PrimaryImage_DataSize_bits
		(BITPIX : Integer;
		 NAXIS  : Strict.Positive_Arr) return Positive;


	-- implements [FITS] Eq(2)
	
	function ConformingExtension_DataSize_bits
		(BITPIX : Integer;
		 NAXIS  : Strict.Positive_Arr;
		 PCOUNT : Natural;
		 GCOUNT : Positive) return Positive;


	-- implements [FITS] Eq(4) 

	 function RandomGroups_DataSize_bits
		(BITPIX : Integer;
		 NAXIS  : Strict.Positive_Arr;
		 PCOUNT : Natural;
		 GCOUNT : Positive) return Positive;


end Formulas;
