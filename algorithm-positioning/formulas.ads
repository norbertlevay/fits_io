
with Keyword_Record; use Keyword_Record;
with FA_Primary;
with FA_Extension;


package Formulas is

	type Positive_Count is new Positive;

	-- Calc size

	function  Calc_HDU_Size_blocks
		(PrimSize : in FA_Primary.Result_Rec) return Positive_Count;
-- FIXME PCOUNT GCOUNT not implemented yet

	function  Calc_HDU_Size_blocks
		(HDUSizeInfo : in FA_Extension.Result_Rec) return Positive_Count;


	function  Calc_HeaderUnit_Size_blocks
		(CardsCount : in Positive) return Positive_Count;

	function  Calc_DataUnit_Size_blocks  
		(BITPIX   : in Integer;--Data_Type;
		 NAXISArr : in Positive_Arr) return Positive_Count;
	
end Formulas;


