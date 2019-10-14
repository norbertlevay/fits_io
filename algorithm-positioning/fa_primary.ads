
with FITS; use FITS;
-- Card_Type HDU_Size_Info needed


package FA_Primary is

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
	function  Next(Pos : Positive; Card : Card_Type) return Natural;

	function  Get return HDU_Size_Rec;

	Unexpected_Card       : exception;
 	Unexpected_Card_Value : exception;
	Card_Not_Found        : exception;
	Programming_Error     : exception;

end FA_Primary;

-- NOTE
--
-- Lexar (Configuration/Reset_State/Next): 
-- from Heaeder cards select only those which 
-- are needed for HDU_Size_Rec e.g. size calculations:
-- such data structure is defined by standard and so can be
-- statically encoded beforehand

