
with FITS;  use  FITS; -- Card_Type *_Max needed


package FA_Extension is

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
        function  Next (Pos  : in Positive; Card : in Card_Type) return Natural;

	Unexpected_Card       : exception;
	Unexpected_Card_Value : exception;
	Programming_Error     : exception;

	--
	-- definition of states
	--

type State_Name is
        (NOT_ACCEPTING_CARDS,  -- FA inactive
         CONFORMING_EXTENSION, -- Initial state: collect scalar card-values
         COLLECT_TABLE_ARRAYS, -- collect TFORM & TBCOL arrays and END-card
         WAIT_END,             -- ignore all cards except END-card
         SPECIAL_RECORDS,IMAGE,TABLE,BINTABLE); -- Final states

type XT_Type is
        (UNSPECIFIED, IMAGE, ASCII_TABLE, BIN_TABLE);

type CardValue is
        record
                Value : String(1..20);
                Read  : Boolean;
        end record;

type NAXIS_Arr   is array (1..NAXIS_Max)   of CardValue;
type TFIELDS_Arr is array (1..TFIELDS_Max) of CardValue;

type State_Type is
        record
	Name         : State_Name;
	XTENSION_Val : XT_Type;
	NAXIS_Val    : Natural;
        TFIELDS_Val  : Natural;

        XTENSION : CardValue;
        BITPIX   : CardValue;
        NAXIS    : CardValue;
        NAXISn   : NAXIS_Arr;
        PCOUNT   : CardValue;
        GCOUNT   : CardValue;
        TFIELDS  : CardValue;
        TFORMn   : TFIELDS_Arr;
        TBCOLn   : TFIELDS_Arr;
        ENDCardPos : Natural;
        ENDCardSet : Boolean;
        end record;

	function  Get return State_Type;

end FA_Extension;

