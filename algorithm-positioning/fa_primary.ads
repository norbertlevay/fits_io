
with FITS; use FITS;

package FA_Primary is

	-- Mandatory keys

	type Primary_HDU is
		(NO_DATA, IMAGE, RANDOM_GROUPS);

	type Result_Rec(HDU  : Primary_HDU;
			Last : Natural) is
		record
			CardsCount : Positive;
			BITPIX     : Integer;
			case HDU is
				when IMAGE | RANDOM_GROUPS =>
					NAXISArr : NAXIS_Arr(1 .. Last);
					case HDU is
					when RANDOM_GROUPS =>
						PCOUNT : Natural;
						GCOUNT : Positive;
					when others => null;
					end case;
				when others => null;
			end case;
		end record;



	function Reset_State return Positive;
	function Next(Pos : in Positive; Card : in Card_Type) return Natural;
	function Get return Result_Rec;


	Unexpected_Card       : exception;
	Unexpected_Card_Value : exception;
	Duplicate_Card        : exception;
	Card_Not_Found        : exception;
	Invalid_Card	      : exception;
	Programming_Error     : exception;

end FA_Primary;

