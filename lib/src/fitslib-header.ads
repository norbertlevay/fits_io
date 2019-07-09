--
-- Describes (through type-records) what information is available
-- in the Header.
--
-- Provides parse and compose procedures to convert between 
-- array-of-cards <-> ada-record-fields.
--
-- The array does not need to be complete, only those
-- record-fields are filled in whose cards where found in the array, 
-- e.g. ada-record might leave some fields at init value:
-- repeat Parse() for all avalable cards (e.g. complete header). 

-- FIXME error/exception handling missing
-- FIXME introduce FITS_Character as defined in [FITSxxx]
-- NOTE Data Size: length of Data (without padding).
-- NOTE DataUnit Size: length of DataUnit (including padding).

-- FIXME change XxxxSize_Type to XxxxDimensions_Type or similar: 
-- size is calculated from dimensions stored in these structtures

-- NOTE whether ONE Vriant-record and 2 funcs: 
-- parse discriminants + parse variant record(for given discriminants)
-- or having MANY (non-variant) records and parse record funcs
-- is implementation detail



--
-- this spec contains groups of triple:
-- -- type XY is ...
-- -- Parse(Card_Arr; in out XY);
-- -- Compose(XY) return Card_Arr;
--


package FITSlib.Header is
	
	subtype CardRange    is Integer range  1 .. 80;

	subtype NameRange    is Integer range  1 ..  8;
	subtype ValueRange   is Integer range 11 .. 30;
	subtype CommentRange is Integer range 33 .. 80;
 
	subtype Card_Type  is String(CardRange);
	type    Card_Arr   is array (Positive range <>) of Card_Type;
	
	CardsPerBlock : constant Positive := 36;
	--BlockSize : constant Positive := CardsPerBlock * 80;
	
	subtype Card_Block is Card_Arr(1..CardsPerBlock);



 	-- HDU determination
	
	type HDU_Variant is 
		(UNKNOWN,          -- nor SIMPLE nor XTENSION found 
		 PRIM_UNKNOWN,     -- SIMPLE found but value not F nor T
		 PRIM_NON_STANDARD,-- SIMPLE = F
		 PRIM_NO_DATA,     -- SIMPLE = T and NAXIS  = 0 
		 PRIM_IMAGE,       -- SIMPLE = T and NAXIS != 0
		 RAND_GROUPS,      -- SIMPLE = T and NAXIS != 0 and NAXIS1 = 0
		 EXT_IMAGE,        -- XTENSION = IMAGE
		 EXT_TABLE,        -- XTENSION = TABLE
		 EXT_BINTABLE,     -- XTENSION = BINTABLE
		 EXT_UNKNOWN);     -- XTENSION found but value none of above

	type Std_Prim is new HDU_Variant range PRIM_NO_DATA .. PRIM_IMAGE;
	--type Conf_Ext is new HDU_Variant range EXT_IMAGE .. EXT_BINTABLE;

	function Parse (Cards  : Card_Arr) return HDU_Variant;



	-- For header size calculation

	ENDCard : constant Card_Type := ( 1=>'E', 2=>'N', 3=>'D', others => ' ');

	type HeaderSize_Type is
		record
			ENDCardFound : Boolean;
			CardCount    : Positive;
		end record;
	
	procedure Parse
		(Cards : Card_Arr;
		 Keys  : in out HeaderSize_Type);




	-- FIXME for mem usage optimization provide NAXISn arr max length by generic
	NAXIS_Last : constant := 999;
	subtype NAXIS_Range is Positive range 1 .. NAXIS_Last;
	subtype NAXIS_Type  is Natural  range 0 .. NAXIS_Last;
	-- Primary HDU has no data if NAXIS=0
	type    NAXIS_Arr  is array (NAXIS_Range range <>) of Positive;
	

	-- Primary with IMAGE
	-- note: cannot be callled Primary_Type, because 
	-- Primary HDU may have no data (e.g. implies no NAXIS_Arr in record)
         
	type Primary_Image_Type is
                record
                        BITPIX : Integer;
                        NAXIS  : NAXIS_Type;
                        NAXISn : NAXIS_Arr(NAXIS_Range);
                end record;

        procedure Parse
                (Cards : Card_Arr;
                 Keys  : in out Primary_Image_Type);

        procedure Compose
                (Keys  : Primary_Image_Type;
                 Cards : out Card_Arr) is null;




	-- Primary with RandomGroups

	-- FIXME differs from ConformingExt in NAXIS1/Natural & NAXISn(2...Last)/Positive 
		 -- - implement it!
       type Random_Groups_Type is
                record
                        BITPIX : Integer;
                        NAXIS  : NAXIS_Range;
                        NAXIS1 : NAXIS_Type;
                        NAXISn : NAXIS_Arr(2 .. NAXIS_Range'Last);
			PCOUNT : Natural;
			GCOUNT : Positive;
                end record;

        procedure Parse
                (Cards : Card_Arr;
                 Keys  : in out Random_Groups_Type);

	-- no Compose()
	-- no support for RandomGroups:
	-- we need only Parse to determine DataUnit-size 
	-- to be able to read other HDU's following 
	-- RandGroups if present in the same file



       type Conforming_Extension_Type is
                record
                        BITPIX : Integer;
                        NAXIS  : NAXIS_Range;
                        NAXISn : NAXIS_Arr(NAXIS_Range);
			PCOUNT : Natural;
			GCOUNT : Positive;
                end record;

        procedure Parse
                (Cards : Card_Arr;
                 Keys  : in out Conforming_Extension_Type);

        procedure Compose
                (Keys  : Conforming_Extension_Type;
                 Cards : out Card_Arr) is null;



	-- FIXME what to do with this ? Can be used for list() -> yes, use it as "common" type
		 -- to calc data size (common = uniting all 3 calc formulas into one: Conf_Ext)
	-- For common data size calculation
	

	type DataSize_Type is
		record
			SIMPLE   : String(ValueRange);
			GROUPS   : String(ValueRange);
			XTENSION : String(ValueRange);
			BITPIX : Integer;
			NAXIS  : NAXIS_Type;
			NAXISn : NAXIS_Arr(NAXIS_Range);
			-- FIXME NAXIS1=0 if RandGroups: make separate and let NAXIS run from 2...NAXIS
			PCOUNT : Natural;
			GCOUNT : Positive;
		end record;

	EmptyCardValue : constant String(ValueRange) := (others =>  ' ');
	DataSize_Null  : constant DataSize_Type := (EmptyCardValue,
	                                            EmptyCardValue,
						    EmptyCardValue,
						    0, 0, (others => 0), 0, 1);

	procedure Parse
		(Cards : Card_Arr;
		 Keys  : in out DataSize_Type);
	
	procedure Compose
		(Keys  : DataSize_Type;
		 Cards : Card_Arr);


	



end FITSlib.Header;
