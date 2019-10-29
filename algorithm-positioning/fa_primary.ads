
with FITS; use FITS; -- Card_Type needed
with Reserved; use Reserved;

package FA_Primary is

	-- Mandatory keys

	type Primary_HDU is (NO_DATA, IMAGE, RANDOM_GROUPS);	
	type Primary_Size_Rec(Last : Natural;
        	           HDUType : Primary_HDU) is
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


	-- Reserved keys

	type Res_Key_Arr is array (Positive range <>) of Reserved_Key;

	type Key_Rec is 
		record
			Key   : Reserved_Key;
			Value : String(1..20);
		end record;
	type Key_Rec_Arr is array (Positive range <>) of Key_Rec;


	-- RANDOM GROUPS related
	
	type RG_KeyRoot is (PTYPE,PSCAL,PZERO);-- array roots
	type RANDG_Arr is array (1..RANDG_Max) of CardValue;
	type RG_Card_Data is 
		record
			Key   : RG_KeyRoot;
			Value : RANDG_Arr;
		end record;
	type RG_Arr is
		array (Positive range <>) of RG_Card_Data;


-- FA configuration
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

	function Get return Primary_Size_Rec;
	function Get(Keys     : in Res_Key_Arr) return Key_Rec_Arr;
--	function Get(KeyRoots : in Res_Key_Arr) return Reserved_Arrays_Arr;
	function Get return RG_Arr;


	Unexpected_Card       : exception;
 	Unexpected_Card_Value : exception;
	Duplicate_Card        : exception;
	Card_Not_Found        : exception;
	Invalid_Card	      : exception;
	Programming_Error     : exception;

end FA_Primary;

