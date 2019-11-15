
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Keyword_Record;

package Strict is

	type HDU_Type is
		(NO_DATA, IMAGE, RANDOM_GROUPS, -- Primary
		CONFORMING_EXTENSION,
		STANDARD_IMAGE, STANDARD_TABLE, STANDARD_BINTABLE);
	
	type TFORM_Arr is array (Positive range <>) of Unbounded_String;

	type Result_Rec(HDU : HDU_Type;
			NAXIS_Last   : Natural;
			TFIELDS_Last : Natural) is
		record
			CardsCount : Positive;
			BITPIX     : Integer;

                        case HDU is
                        when IMAGE .. STANDARD_BINTABLE  =>
				NAXISArr : Keyword_Record.Positive_Arr(1 .. NAXIS_Last);
                                case HDU is
                                when RANDOM_GROUPS .. STANDARD_BINTABLE =>
                                	PCOUNT : Natural;
                                        GCOUNT : Positive;

					case HDU is
					when STANDARD_TABLE | STANDARD_BINTABLE =>
						TFORMn : TFORM_Arr(1..TFIELDS_Last);
						case HDU is
						when STANDARD_TABLE =>
					     	TBCOLn : Keyword_Record.Positive_Arr(1..TFIELDS_Last);
						when others => null;
						end case;
				 	when others => null;
				 	end case;

                                 when others => null;
                                 end case;
                        when others => null;
                        end case;
 		end record;


	function Reset_State return Positive; 
	function Next (Pos : in Positive; Card : in Keyword_Record.Card_Type) return Natural;
	function Get return Result_Rec;


	Unexpected_First_Card : exception;-- possibly Special Records
	Unexpected_Card       : exception;
	Unexpected_Card_Value : exception;
	Duplicate_Card        : exception;
	Card_Not_Found        : exception;
	Invalid_Card          : exception;
	Programming_Error     : exception;

end Strict;

