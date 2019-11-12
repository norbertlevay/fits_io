
with FITS; use FITS;

package FA_Extension is

	-- Mandatory keys

	type Extension_HDU is
		(CONFORMING_EXTENSION,
		STANDARD_IMAGE, STANDARD_TABLE, STANDARD_BINTABLE,
		SPECIAL_RECORDS);

	type Size_Rec(Last : Positive) is
		record
			HDUType    : Extension_HDU;
			CardsCount : Positive;
			BITPIX     : Integer;
			NAXISArr   : NAXIS_Arr(1 .. Last);
			PCOUNT     : Natural;
			GCOUNT     : Positive;
		end record;


	-- FA configuration
	type Algorithm_Type is
		(
			ALGORITHM_STRICT,      -- parsing Headers follows strictly FITS-Standard
			ALGORITHM_TOLERANT     -- parsing Headers fails only if: 
			-- * essential key is missing
			-- * essential key is duplicate with different values (ambiguity) 
			); -- FIXME not implemented


	type Options_Type is
		record
			Mand    : Boolean;
			Tab     : Boolean;
			ConfExt : Boolean;
			Reserved : Boolean;
		end record;


	--
	-- finite automaton
	--

	procedure Configure(Options : Options_Type);

	function  Reset_State return Positive; 
	function  Next (Pos  : in Positive; Card : in Card_Type) return Natural;

	function Get return Size_Rec;


	Unexpected_Card       : exception;
	Unexpected_Card_Value : exception;
	Duplicate_Card        : exception;
	Card_Not_Found        : exception;
	Invalid_Card          : exception;
	Programming_Error     : exception;

end FA_Extension;

