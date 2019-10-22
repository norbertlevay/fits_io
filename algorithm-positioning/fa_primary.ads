
with FITS; use FITS; -- Card_Type needed
with Reserved; use Reserved; -- Obs_Key needed

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
	
	-- Reserved keys: data array (IMAGE) related
type Card_Data is 
	record
		Key   : DataArr_Key;
		Value : String(1..20);
	end record;
type DataArr_Arr is
	array (Positive range <>) of Card_Data;

	function Get return DataArr_Arr;



	-- Reserved keys: observation related
type Obs_Card_Data is 
	record
		Key   : Obs_Key;
		Value : String(1..20);
	end record;
type Obs_Arr is
	array (Positive range <>) of Obs_Card_Data;

	function Get return Obs_Arr;





	Unexpected_Card       : exception;
 	Unexpected_Card_Value : exception;
	Duplicate_Card        : exception;
	Card_Not_Found        : exception;
	Invalid_Card	      : exception;
	Programming_Error     : exception;

end FA_Primary;

