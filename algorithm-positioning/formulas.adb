
package body Formulas is

	 function  Calc_HDU_Size_blocks
                (PrimSize : in FA_Primary.Primary_Size_Rec) return Positive_Count
	 is
                HDUSize : Positive_Count;
	 begin
                HDUSize :=
                        Calc_HeaderUnit_Size_blocks(PrimSize.CardsCount)
                        +
                        Calc_DataUnit_Size_blocks(PrimSize.BITPIX, PrimSize.NAXISArr);

                return HDUSize;
		 
	 end Calc_HDU_Size_blocks;






	function  Calc_HDU_Size_blocks
		(HDUSizeInfo : in HDU_Size_Rec) return Positive_Count
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
		HUSize := Positive_Count(1 + (CardsCount - 1)/36);
		return HUSize;
	end Calc_HeaderUnit_Size_blocks;


	function  Calc_DataUnit_Size_blocks  
		(BITPIX   : in Integer;--Data_Type;
		 NAXISArr : in NAXIS_Arr) 
		 return Positive_Count
	is
		DUSize : Positive_Count := 1;
		BlockSize_bits : constant Positive_Count := 2880*8;
		DataInBlock    : constant Positive_Count := BlockSize_bits /Positive_Count(abs BITPIX);
		DUSizeInBlocks : Positive_Count;
	begin
               for I in NAXISArr'Range
                loop
                        DUSize := DUSize * Positive_Count(NAXISArr(I));
                end loop;

		DUSizeInBlocks := 1 + (DUSize - 1) / DataInBlock;

		return DUSizeInBlocks;
	end Calc_DataUnit_Size_blocks;
	
end Formulas;


