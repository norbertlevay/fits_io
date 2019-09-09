
package body Formulas is

	function  Calc_HDU_Size_blocks
		(HDUSizeInfo : in HDU_Size_Info_Type) return Positive_Count
	is
		HDUSize : Positive_Count;
	begin
		HDUSize :=  
			Calc_HeaderUnit_Size_blocks(HDUSizeInfo.CardsCount) 
			+
			Calc_DataUnit_Size_blocks(HDUSizeInfo.BITPIX, HDUSizeInfo.NAXISArr);

		return HDUSize;
	end Calc_HDU_Size_blocks;
	


	function  Calc_HeaderUnit_Size_blocks
		(CardsCount : in Positive) 
		return Positive_Count
	is
		HUSize : Positive_Count;
	begin
		return HUSize;
	end Calc_HeaderUnit_Size_blocks;


	function  Calc_DataUnit_Size_blocks  
		(BITPIX   : in Data_Type;
		 NAXISArr : in NAXIS_Arr) 
		 return Positive_Count
	is
		DUSize : Positive_Count;
	begin
		return DUSize;
	end Calc_DataUnit_Size_blocks;
	
end Formulas;


