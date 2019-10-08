
with FITS; use FITS;
-- Card_Type HDU_Size_Info_Type needed

package Primary_Size_Info is

type Options_Type is 
	(DONT_STORE_CARD_COMMENTS, 
	 COLLECT_HISTORY_CARDS, 
	 COLLECT_COMMENT_CARDS,
	 ALGORITHM_STRICT,      -- parsing Headers follows strictly FITS-Standard
	 ALGORITHM_TOLERANT);   -- parsing Headers fails only if: 
				-- * essential key is missing
				-- * essential key is duplicate with different values (ambiguity) 

	--
	-- state machine
	--

	procedure Configuration(Options : Options_Type) is null;
	procedure Reset_State;
	function  Next(Pos : Positive; Card : Card_Type) return Natural;


	--
	-- collect results
	--
	
type CardValue is
        record
                Value : String(1..20);
                Read  : Boolean;
        end record;

type NAXIS_Arr is array (1..NAXIS_Last) of CardValue;

type Primary_Mandatory_Card_Values is
        record
        SIMPLE : CardValue;
        BITPIX : CardValue;
        NAXIS  : CardValue;
        NAXIS1 : CardValue;
        NAXISn : NAXIS_Arr;
        PCOUNT : CardValue;
        GCOUNT : CardValue;
        GROUPS : CardValue;
        ENDCardPos : Natural;
        ENDCardSet : Boolean;
        end record;

	function  Get return Primary_Mandatory_Card_Values;





	-- FIXME move elsewhere
	function  Get return HDU_Size_Info_Type;


end Primary_Size_Info;

-- NOTE
--
-- Lexar (Configuration/Reset_State/Next): 
-- from Heaeder cards select only those which 
-- are needed for HDU_Size_Info_Type e.g. size calculations:
-- such data structure is defined by standard and so can be
-- statically encoded beforehand
--
-- Grammar (Get HDU_Size_Info_Type): 
	-- analogue to JSON or XML+DTD
	-- In FITS header variables have implicit types: 
	-- a variable name (Card.Key) implies type as defined in standard
	-- covers rules on relationships between cards:
	-- * individual cards, -> like varibles of basic types
	-- * card-arrays, -> like arrays of basic types
	-- * card-sets, -> like structs/records
	-- * alternative sets -> like ??



