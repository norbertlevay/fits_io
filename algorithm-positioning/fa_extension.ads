
with FITS;  use  FITS; 
-- Card_Type *_Max HDU_Size_Info needed


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
	-- finite automaton
	--

	procedure Configuration(Options : Options_Type) is null;

	function  Reset_State return Positive; 
        function  Next (Pos  : in Positive; Card : in Card_Type) return Natural;

	function  Get return HDU_Size_Info_Type;
	
	Unexpected_Card       : exception;
	Unexpected_Card_Value : exception;
	Card_Not_Found        : exception;
	Programming_Error     : exception;

end FA_Extension;

