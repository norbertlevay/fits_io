--
-- Various calculations formulas from [FITS].
--
-- FIXME error/exception handling missing
-- FIXME rename/re-examine FPositive FNatural definition in FITSlib.ads

with FITSlib.Header; use FITSlib.Header;

package FITSlib.Formulas is

	type    FInteger  is range -(2**63) .. +(2**63 - 1);
	-- 64bit portable, guaranteed to be 64bit or will not compile
	subtype FNatural  is FInteger range 0 .. FInteger'Last;
	subtype FPositive is FNatural range 1 .. FNatural'Last;
	
	--
	-- Data array size calculations for all HDU types
	--

	-- implements [FITS] Eq(1)
	
	function PrimaryHDU_DataSize_bits
		(BITPIX : Integer;
		 NAXIS  : NAXIS_Arr) return FPositive;


	-- implements [FITS] Eq(2)
	
	function ConformingExtension_DataSize_bits
		(BITPIX : Integer;
		 NAXIS  : NAXIS_Arr;
		 PCOUNT : FNatural;
		 GCOUNT : FPositive) return FPositive;


	-- implements [FITS] Eq(4) 

	 function RandomGroups_DataSize_bits
		(BITPIX : Integer;
		 NAXIS  : NAXIS_Arr;
		 PCOUNT : FNatural;
		 GCOUNT : FPositive) return FPositive;


end FITSlib.Formulas;
