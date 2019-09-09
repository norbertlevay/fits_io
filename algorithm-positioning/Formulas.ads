
package Formulas is

	-- Calc size
	
	function  Header_Size_sioUnits(CardsCount : in Positive) return SIO.Positive_Count;
	function  Data_Size_sioUnits  
		(BITPIX   : in Data_Type;
		 NAXISArr : in NAXIS_Arr) return SIO.Positive_Count;
	
	-- utils used for the 2 funcs above
	
	function  Header_Size_cardcnt(Size : in HDU_Size_Info_Type) return Formulas.Positive_Count;
	function  Data_Size_bits(Size : in HDU_Size_Info_Type) return Formulas.Positive_Count;

	function HeaderUnit_Size_blocks(CardCount : in Formulas.Positive_Count) 
		return Formulas.Positive.Count;

	function DataUnit_Size_blocks(DataSize_bits : in Formulas.Positive_Count) 
		return Formulas.Positive.Count;

	function Block_Size_sioUits(BlockCnt : in Formulas.Positive_Count) 
		return Formulas.Positive.Count;

end Formulas;


