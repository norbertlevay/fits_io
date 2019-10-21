
with FITS; use FITS; -- Card_Type needed


package FA_Primary is

type Algorithm_Type is 
	(
	ALGORITHM_STRICT,      -- parsing Headers follows strictly FITS-Standard
	ALGORITHM_TOLERANT   -- parsing Headers fails only if: 
				-- * essential key is missing
				-- * essential key is duplicate with different values (ambiguity)
	); -- FIXME not implemented


type Options_Type is
	record
		Mand   : Boolean;
		Reserved : Boolean;
	end record;



	--
	-- finite automaton
	--

	procedure Configure(Options : Options_Type);

	function  Reset_State return Positive;
	function  Next(Pos : Positive; Card : Card_Type) return Natural;

	function  Get return HDU_Size_Rec;

type Res_Arr_Keys is (BSCALE,BZERO,BUNIT,BLANK,DATAMAX,DATAMIN);
type Card_Data is 
	record
		Key   : Res_Arr_Keys;
		Value : String(1..20);
	end record;
type ImData_Arr is
	array (Positive range <>) of Card_Data;

	function Get return ImData_Arr;





	Unexpected_Card       : exception;
 	Unexpected_Card_Value : exception;
	Duplicate_Card        : exception;
	Card_Not_Found        : exception;
	Invalid_Card	      : exception;
	Programming_Error     : exception;

end FA_Primary;

