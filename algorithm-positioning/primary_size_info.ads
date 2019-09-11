--
-- Collecting Mandatory keys needed to calculate Primary HDU size
--

package Primary_Size_Info is


type Options_Type is 
	(DONT_STORE_CARD_COMMENTS, 
	 COLLECT_HISTORY_CARDS, 
	 COLLECT_COMMENT_CARDS,
	 ALGORITHM_STRICT,      -- parsing Headers follows strictly FITS-Standard
	 ALGORITHM_TOLERANT);   -- parsing Headers fails only if: 
				-- * essential key is missing
				-- * essential key is duplicate with different values (ambiguity) 

type Read_Control is 
	(Continue,           -- continue calling Next() and supplying CardBlocks
	 StartFromBegining,  -- read again CardBlock from begining of Header
	 Stop);              -- do not provide more CardBlocks, usually after END-card found
-- this enables implement various parsing strategies including 2-pass parsing (StartFromBegining)

type Card_Type is new String(1..80);
type Card_Block is array(1..36) of Card_Type;
ENDCard  : constant Card_Type := "END                                                                             ";

NAXIS_Last : Positive := 9;
type NAXIS_Arr is array(1..NAXIS_Last) of Positive;

-- Lexar: 
-- from Heaeder cards select only those which 
-- are needed for HDU_Size_Info_Type e.g. size calculations:
-- such data structure is defined by standard and so can be
-- statically encoded beforehand

	procedure Configuration(Options : Options_Type) is null;
	procedure Reset_State;
	function  Next(BlockNum : Positive; CardBlock : Card_Block) return Read_Control;



-- Grammar: 
	-- analogue to JSON or XML+DTD
	-- In FITS header variables have implicit types: 
	-- a variable name (Card.Key) implies type as defined in standard
	-- covers rules on relationships between cards:
	-- * individual cards, -> like varibles of basic types
	-- * card-arrays, -> like arrays of basic types
	-- * card-sets, -> like structs/records
	-- * alternative sets -> like ??

type HDU_Type is 
	(PRIMARY_IMAGE, RANDOM_GROUPS, EXT_IMAGE, EXT_ASCII_TABLE, BIN_TABLE, UNSPECIFIED_EXTENSION);

--type Data_Type is 
--	(INT8, INT16, INT32, INT64, FLOAT32, FLOAT64);

type HDU_Size_Info_Type is 
	record
		HDUType    : HDU_Type;   -- HDU info
		CardsCount : Positive;   -- HDU info
		BITPIX     : Integer; --Data_Type;  -- HDU info
		NAXISArr   : NAXIS_Arr;  -- HDU info
	end record;

	function  Get return HDU_Size_Info_Type;


end Primary_Size_Info;


