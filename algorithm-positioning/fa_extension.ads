
with FITS; use FITS; -- Card_Type needed
with Reserved; use Reserved;

package FA_Extension is



        -- Reserved keys

        type Res_Key_Arr is array (Positive range <>) of Reserved_Key;

        type Key_Rec is
                record
                        Key   : Reserved_Key;
                        Value : String(1..20);
                end record;
        type Key_Rec_Arr is array (Positive range <>) of Key_Rec;




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

	function Get return HDU_Size_Rec;
	function Get(Keys : in Res_Key_Arr) return Key_Rec_Arr;

	type CardValue is
        record
                Value : String(1..20);
                Read  : Boolean;
        end record;

	type TFIELDS_Arr is array (1..TFIELDS_Max) of CardValue;


        type Root_Type is (TTYPE,TUNIT,TSCAL,TZERO,TNULL,TDISP);
        type Root_Arr  is array (Natural range <>) of Root_Type;


        type IdxKey_Rec is
                record
                        Root : Root_Type;
                        Arr  : Reserved.TFIELDS_Arr;
                end record;
        type IdxKey_Rec_Arr is array (Natural range <>) of IdxKey_Rec;

	function  Get(Roots : Root_Arr) return IdxKey_Rec_Arr;
	
	Unexpected_Card       : exception;
	Unexpected_Card_Value : exception;
	Duplicate_Card        : exception;
	Card_Not_Found        : exception;
	Invalid_Card          : exception;
	Programming_Error     : exception;

end FA_Extension;

