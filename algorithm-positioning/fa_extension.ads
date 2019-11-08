
with FITS; use FITS;

package FA_Extension is

	-- Mandatory keys

	type Extension_HDU is
		(CONFORMING_EXTENSION,
		STANDARD_IMAGE, STANDARD_TABLE, STANDARD_BINTABLE,
		SPECIAL_RECORDS);

	type HDU_Size_Rec(Last : Positive) is
		record
			HDUType    : Extension_HDU;
			CardsCount : Positive;
			BITPIX     : Integer;
			NAXISArr   : NAXIS_Arr(1 .. Last);
			PCOUNT     : Natural;
			GCOUNT     : Positive;
		end record;





	-- Reserved keys

	type Res_Key_Arr is array (Positive range <>) of Reserved_Key;

	type Key_Rec is
		record
			Key   : Reserved_Key;
			Value : String(1..20);
		end record;
	type Key_Rec_Arr is array (Positive range <>) of Key_Rec;

	-- Reserved indexed keys 

	type Reserved_Root is (TTYPE,TUNIT,TSCAL,TZERO,TNULL,TDISP);

	type Res_Root_Arr  is array (Natural range <>) of Reserved_Root;

	type IdxKey_Rec is
		record
			Root : Reserved_Root;
			Arr  : TFIELDS_Arr;
		end record;
	type IdxKey_Rec_Arr is array (Natural range <>) of IdxKey_Rec;


	-- FA configuration
	type Algorithm_Type is
		(
			ALGORITHM_STRICT,      -- parsing Headers follows strictly FITS-Standard
			ALGORITHM_TOLERANT     -- parsing Headers fails only if: 
			-- * essential key is missing
			-- * essential key is duplicate with different values (ambiguity) 
			); -- FIXME not implemented


	type Options_Type is
		record
			Mand    : Boolean;
			Tab     : Boolean;
			ConfExt : Boolean;
			Reserved : Boolean;
		end record;


	--
	-- finite automaton
	--

	procedure Configure(Options : Options_Type);

	function  Reset_State return Positive; 
	function  Next (Pos  : in Positive; Card : in Card_Type) return Natural;

	function Get return HDU_Size_Rec;
	function Get(Keys  : in Res_Key_Arr)  return Key_Rec_Arr;
	function Get(Roots : in Res_Root_Arr) return IdxKey_Rec_Arr;



	Unexpected_Card       : exception;
	Unexpected_Card_Value : exception;
	Duplicate_Card        : exception;
	Card_Not_Found        : exception;
	Invalid_Card          : exception;
	Programming_Error     : exception;

end FA_Extension;

