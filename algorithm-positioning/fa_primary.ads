
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

--	function  Get return HDU_Size_Rec;


	-- Mandatory
type Primary_HDU is (NO_DATA, IMAGE, RANDOM_GROUPS);	
--type NAXISn_Arr is array (Positive range <>) of Positive;
type Primary_Size_Rec(Last : Natural;
                HDUType    : Primary_HDU) is
        record
                CardsCount : Positive;
                BITPIX     : Integer;
                NAXISArr   : NAXIS_Arr(1 .. Last);
		case HDUType is
			when RANDOM_GROUPS =>
				PCOUNT : Natural;
				GCOUNT : Positive;
			when others =>
				null;
		end case;
        end record;

	function  Get return Primary_Size_Rec;



	-- Reserved keys: production
type Prod_Card_Data is 
	record
		Key   : Prod_Key;
		Value : String(1..20);
	end record;
type Prod_Arr is
	array (Positive range <>) of Prod_Card_Data;

	function Get return Prod_Arr;

	-- Reserved keys: bibliographic
type Biblio_Card_Data is 
	record
		Key   : Biblio_Key;
		Value : String(1..20);
	end record;
type Biblio_Arr is
	array (Positive range <>) of Biblio_Card_Data;

	function Get return Biblio_Arr;

	-- Reserved keys: observation related
type Obs_Card_Data is 
	record
		Key   : Obs_Key;
		Value : String(1..20);
	end record;
type Obs_Arr is
	array (Positive range <>) of Obs_Card_Data;

	function Get return Obs_Arr;

	-- Reserved keys: data array (IMAGE) related
type DataArr_Card_Data is 
	record
		Key   : DataArr_Key;
		Value : String(1..20);
	end record;
type DataArr_Arr is
	array (Positive range <>) of DataArr_Card_Data;

	function Get return DataArr_Arr;

	-- Reserved keys: RANDOM GROUPS releated
type RG_KeyRoot is (PTYPE,PSCAL,PZERO);-- array roots
type RANDG_Arr is array (1..RANDG_Max) of CardValue;
type RG_Card_Data is 
	record
		Key   : RG_KeyRoot;
		Value : RANDG_Arr;
	end record;
type RG_Arr is
	array (Positive range <>) of RG_Card_Data;

	function Get return RG_Arr;



	Unexpected_Card       : exception;
 	Unexpected_Card_Value : exception;
	Duplicate_Card        : exception;
	Card_Not_Found        : exception;
	Invalid_Card	      : exception;
	Programming_Error     : exception;

end FA_Primary;

