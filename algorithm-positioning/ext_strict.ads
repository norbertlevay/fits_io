
with FITS;  use  FITS; -- Card_Type needed


package Ext_Strict is

type Options_Type is
         (DONT_STORE_CARD_COMMENTS,
          COLLECT_HISTORY_CARDS,
          COLLECT_COMMENT_CARDS,
	  ALGORITHM_STRICT,      -- parsing Headers follows strictly FITS-Standard
          ALGORITHM_TOLERANT);   -- parsing Headers fails only if: 
                                 -- * essential key is missing
                                 -- * essential key is duplicate with different values (ambiguity) 
	--
	-- state machine
	--

	procedure Configuration(Options : Options_Type) is null;
	procedure Reset_State; 
        function  Next (Pos  : in Positive; Card : in Card_Type) return Natural;


	--
	-- collect results
	--

type CardValue is
        record
                Value : String(1..20);
                Read  : Boolean;
        end record;

type NAXIS_Arr is array (1..NAXIS_Last) of CardValue;
type TFORM_Arr is array (1..TFIELDS_Max) of CardValue;
type TBCOL_Arr is array (1..TFIELDS_Max) of CardValue;


type Extension_Mandatory_Card_Values is
        record
        XTENSION : CardValue;
        BITPIX   : CardValue;
        NAXIS    : CardValue;
        NAXISn   : NAXIS_Arr;
        PCOUNT   : CardValue;
        GCOUNT   : CardValue;
        TFIELDS  : CardValue;
        TFORMn   : TFORM_Arr;
        TBCOLn   : TBCOL_Arr;
        ENDCardPos : Natural;
        ENDCardSet : Boolean;
        end record;

	function  Get return Extension_Mandatory_Card_Values;

end Ext_Strict;

