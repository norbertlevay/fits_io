
with FITS; use FITS;

package Formulas is

	type Positive_Count is new Positive;

	-- Calc size

	function  Calc_HDU_Size_blocks
		(HDUSizeInfo : in HDU_Size_Rec) return Positive_Count;


	function  Calc_HeaderUnit_Size_blocks
		(CardsCount : in Positive) return Positive_Count;

	function  Calc_DataUnit_Size_blocks  
		(BITPIX   : in Integer;--Data_Type;
		 NAXISArr : in NAXIS_Arr) return Positive_Count;
	
end Formulas;


