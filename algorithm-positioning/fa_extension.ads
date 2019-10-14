
with FITS; use FITS; 
-- Card_Type HDU_Size_Info needed


package FA_Extension is

type Options_Type is
         (ALGORITHM_STRICT,      -- parsing Headers follows strictly FITS-Standard
          ALGORITHM_TOLERANT);   -- parsing Headers fails only if: 
                                 -- * essential key is missing
                                 -- * essential key is duplicate with different values (ambiguity) 
	--
	-- finite automaton
	--

	procedure Configuration(Options : Options_Type) is null;

	function  Reset_State return Positive; 
        function  Next (Pos  : in Positive; Card : in Card_Type) return Natural;

	function  Get return HDU_Size_Rec;
	
	Unexpected_Card       : exception;
	Unexpected_Card_Value : exception;
	Card_Not_Found        : exception;
	Programming_Error     : exception;

end FA_Extension;

