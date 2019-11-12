
with FITS; use FITS;

package FA_Primary is

	-- Mandatory keys

	type Primary_HDU is
		(NO_DATA, IMAGE, RANDOM_GROUPS);

	type Size_Rec(Last : Natural;
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


	-- FA configuration
	type Options_Type is 
		(
			ALGORITHM_STRICT,  -- parsing Headers follows strictly FITS-Standard
			ALGORITHM_TOLERANT -- parsing Headers fails only if: 
			-- * essential key is missing
			-- * essential key is duplicate with different values (ambiguity)
			);

	--
	-- finite automaton
	--

	procedure Configure(Options : Options_Type) is null;

	function  Reset_State return Positive;
	function  Next(Pos : Positive; Card : Card_Type) return Natural;

	function Get return Size_Rec;


	Unexpected_Card       : exception;
	Unexpected_Card_Value : exception;
	Duplicate_Card        : exception;
	Card_Not_Found        : exception;
	Invalid_Card	      : exception;
	Programming_Error     : exception;

end FA_Primary;

