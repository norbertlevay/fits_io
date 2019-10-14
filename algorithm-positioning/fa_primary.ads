
with FITS; use FITS; -- Card_Type needed


package FA_Primary is

type Options_Type is 
	(DONT_STORE_CARD_COMMENTS, 
	 COLLECT_HISTORY_CARDS, 
	 COLLECT_COMMENT_CARDS,
	 ALGORITHM_STRICT,      -- parsing Headers follows strictly FITS-Standard
	 ALGORITHM_TOLERANT);   -- parsing Headers fails only if: 
				-- * essential key is missing
				-- * essential key is duplicate with different values (ambiguity) 
	--
	-- finite automata
	--

	procedure Configuration(Options : Options_Type) is null;
	function  Reset_State return Positive;
	function  Next(Pos : Positive; Card : Card_Type) return Natural;

	Unexpected_Card       : exception;
	Unexpected_Card_Value : exception;
	Programming_Error     : exception;

	--
	-- definition of states
	--

type State_Name is (
        NOT_ACCEPTING_CARDS, -- FA inactive
        PRIMARY_STANDARD,    -- Initial state: collect scalar card-values
        DATA_NOT_IMAGE,      -- collect GROUPS PCOUNT GCOUNT and END-card
        WAIT_END,            -- ignore all cards except END-card
	NO_DATA,IMAGE,RANDOM_GROUPS -- Final states
        );


type CardValue is
        record
                Value : String(1..20);
                Read  : Boolean;
        end record;

type NAXIS_Arr is array (1..NAXIS_Max) of CardValue;


type State_Type is
        record
        Name       : State_Name;
        NAXIS_Val  : Natural;
        NAXIS1_Val : Natural;
		
        SIMPLE : CardValue;
        BITPIX : CardValue;
        NAXIS  : CardValue;
        NAXIS1 : CardValue;
        NAXISn : NAXIS_Arr;
        PCOUNT : CardValue;
        GCOUNT : CardValue;
        GROUPS : CardValue;
        ENDCardPos : Natural;
        ENDCardSet : Boolean;
        end record;

	function  Get return State_Type;

end FA_Primary;

-- NOTE
--
-- Lexar (Configuration/Reset_State/Next): 
-- from Heaeder cards select only those which 
-- are needed for Interpret.HDU_Size_Info_Type e.g. size calculations:
-- such data structure is defined by standard and so can be
-- statically encoded beforehand

