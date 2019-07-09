--
-- Describes (through type-records) what information is available
-- in the Header.
--
-- Provides parse and compose procedures to convert between 
-- array-of-cards <-> ada-record-fields.
--
-- This spec contains groups of triple:
--   type XY is ...
--   Parse(Card_Arr; in out XY);
--   Compose(XY) return Card_Arr;
--
-- The array does not need to be complete, only those
-- record-fields are filled in whose cards where found in the array, 
-- e.g. ada-record might leave some fields at init value:
-- repeat Parse() for all avalable cards (e.g. complete header). 

-- NOTE Data Size: length of Data (without padding).
-- NOTE DataUnit Size: length of DataUnit (including padding).

-- FIXME error/exception handling missing
-- FIXME introduce FITS_Character as defined in [FITSxxx]
-- FIXME change XxxxSize_Type to XxxxDimensions_Type or similar: 
-- size is calculated from dimensions stored in these structtures

-- NOTE whether ONE Vriant-record and 2 funcs: 
-- parse discriminants + parse variant record(for given discriminants)
-- or having MANY (non-variant) records and parse record funcs
-- is _implementation detail_


package FITSlib.Header is
	
	subtype CardRange    is Integer range  1 .. 80;

	subtype NameRange    is Integer range  1 ..  8;
	subtype ValueRange   is Integer range 11 .. 30;
	subtype CommentRange is Integer range 33 .. 80;
 
	subtype Card_Type  is String(CardRange);
	type    Card_Arr   is array (Positive range <>) of Card_Type;
	
	CardsPerBlock : constant Positive := 36;
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

	--type Std_Prim is new HDU_Variant range PRIM_NO_DATA .. PRIM_IMAGE;
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
	type    NAXIS_Arr  is array (NAXIS_Range range <>) of Positive;
	

	

         
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
	-- We need only Parse to determine DataUnit-size 
	-- to be able to read other HDU's following 
	-- RandGroups if present in the same file.
	-- RandomGroups are otherwise not supported.






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




end FITSlib.Header;
