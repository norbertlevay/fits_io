
with FITS; use FITS;

package FA_Extension is

	-- Mandatory keys

	type Extension_HDU is
		(CONFORMING_EXTENSION,
		STANDARD_IMAGE, STANDARD_TABLE, STANDARD_BINTABLE);

	type Result_Rec(HDU : Extension_HDU;
			NAXIS_Last   : Positive;
			TFIELDS_Last : Positive) is
		record
			CardsCount : Positive;
			BITPIX     : Integer;
			NAXISArr   : NAXIS_Arr(1 .. NAXIS_Last);
			PCOUNT     : Natural;
			GCOUNT     : Positive;
			case HDU is
			when STANDARD_TABLE | STANDARD_BINTABLE =>
				TFORMn : TFIELDS_Arr(1..TFIELDS_Last);
				case HDU is
					when STANDARD_TABLE =>
					TBCOLn : TFIELDS_Arr(1..TFIELDS_Last);
					when others => null;
				end case;
			when others => null;
			end case; 
		end record;



	function Reset_State return Positive; 
	function Next (Pos : in Positive; Card : in Card_Type) return Natural;
	function Get return Result_Rec;


	Unexpected_First_Card : exception;-- possibly Special Records
	Unexpected_Card       : exception;
	Unexpected_Card_Value : exception;
	Duplicate_Card        : exception;
	Card_Not_Found        : exception;
	Invalid_Card          : exception;
	Programming_Error     : exception;

end FA_Extension;

