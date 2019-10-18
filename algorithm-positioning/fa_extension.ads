
with FITS; use FITS; 
-- Card_Type HDU_Size_Info needed


package FA_Extension is

type Options_Type is
         (
         NONE, -- no card-group specified: only count valid cards (see [FITS] App. A)
 	 SIZE  -- parse size-related (a.k.a. 'mandatory') cards
--	  ALGORITHM_STRICT,      -- parsing Headers follows strictly FITS-Standard
--        ALGORITHM_TOLERANT     -- parsing Headers fails only if: 
                                 -- * essential key is missing
                                 -- * essential key is duplicate with different values (ambiguity) 
	  );


	--
	-- finite automaton
	--

	procedure Configure(Options : Options_Type);

	function  Reset_State return Positive; 
        function  Next (Pos  : in Positive; Card : in Card_Type) return Natural;

	function  Get return HDU_Size_Rec;
	
	Unexpected_Card       : exception;
	Unexpected_Card_Value : exception;
	Duplicate_Card        : exception;
	Card_Not_Found        : exception;
	Invalid_Card          : exception;
	Programming_Error     : exception;

end FA_Extension;

