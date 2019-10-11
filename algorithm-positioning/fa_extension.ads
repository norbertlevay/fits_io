
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
	-- collect results
	--

type CardValue is
        record
                Value : String(1..20);
                Read  : Boolean;
        end record;

type NAXIS_Arr is array (1..NAXIS_Max)   of CardValue;
type TFIELDS_Arr is array (1..TFIELDS_Max) of CardValue;
--type TFORM_Arr is array (1..TFIELDS_Max) of CardValue;
--type TBCOL_Arr is array (1..TFIELDS_Max) of CardValue;


type Extension_Mandatory_Card_Values is
        record
	SPECRECORDS : Boolean;
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

	function  Get return Extension_Mandatory_Card_Values;

end FA_Extension;

