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
--
-- FIXME error/exception handling missing
-- FIXME introduce FITS_Character as defined in [FITSxxx]
-- NOTE Data Size: length of Data (without padding).
-- NOTE DataUnit Size: length of DataUnit (including padding).

-- FIXME change XxxxSize_Type to XxxxDimensions_Type or similar: 
-- size is calculated from dimensions stored in these structtures


--
-- groups of triple:
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
	subtype Card_Block is Card_Arr(1..CardsPerBlock);


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



	-- For data size calculation
	
	NAXIS_Last : constant := 999;
	subtype NAXIS_Range is Positive range 1 .. NAXIS_Last;
	subtype NAXIS_Type  is Natural  range 0 .. NAXIS_Last;
	-- Primary HDU has no data if NAXIS=0
	type    NAXIS_Arr  is array (NAXIS_Range range <>) of Positive;
	
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
	-- FIXME NAXIS_Type shoul start with 0 or 1?



	procedure Parse
		(Cards : Card_Arr;
		 Keys  : in out DataSize_Type);
	
	procedure Compose
		(Keys  : DataSize_Type;
		 Cards : Card_Arr);



	-- experimental: For Primary Header 
	
	-- Ratio: bacause Primary header has special properties:
	-- -- it is always IMAGE
	-- -- can have no data NAXIS=0 (serves as header for extensions)
	-- -- can have NAXIS1=0 then it contains RandomGroups
	-- Extensions: may be other then IMAGE. But if image NAXIS<0,NAXIS1>0


	-- 1st Header-in-File can be:
	-- non-standard if SIMPLE = 'F'
	-- HeaderForExtensions: SIMPLE='T' & NAXIS = 0 (NAXISn-array cards not present)
	-- RandomGroups: SIMPLE='T' NAXIS=>1 NAXIS1 = 0 (GROUPS='T' and has PCOUNT GCOUNT)
	-- Primary IMAGE: SIMPLE='T' NAXIS=>1 NAXIS1=>1

	-- Q: hide these distinctions (withinSize calcs) or maybe each of these is 
	-- useful to know explicitely for other decisions (than only size calculation).
	-- Standard talks about these HDU types - should the lib-if be explicit on them too:
	-- type Prim_Types enum (NONSTANDARD NODATA RANDGROUPS IMAGE)


	-- NOTE whether ONE Vriant-record and 2 funcs: 
	-- parse discriminants + parse variant record(for given discriminants)
	-- or having MANY (non-variant) records and parse record funcs
	-- is implementation detail
	--
	-- Regardless of implementation 
	-- NON_VARIANT DATA MUST ALWAYS BE DETERMINED(PARESED) FIRST !!!
	-- First nail down the variant part and then procde accordingly...
	--
	-- FIXME: Start even earlier: 
	-- 1st HeaderBlock read:
	-- if FileIndex = 1 -> card(1..8)='SIMPLE' 
	-- -- one PrimaryHDU types (SIMPLE T or F) or not a FITS-file
	-- if FileIndex > 1 -> 
	-- if first card(1..8) = XTENSION -> one of extensions XTENSION=<type>
	-- -- if first card(1..8) key neither SIMPLE nor XTENSION -> unspecified data follows
	--

	type What_File is (NOT_FITS_FILE, STANDARD_PRIMARY, NON_STANDARD_PRIMARY, CONF_EXT);
	-- observes SIMPLE key and its values or SIMPLE not found.
	-- FIXME how to ensure here that FileIndex = 1, e.g. we examin 
	-- cards from begining of the file?
	procedure Parse_First_Block 
		(Cards : Card_Arr;
		 What  : out What_File);
	-- after call 'What' is always valid

	type Standard_Primary_Type is (NO_DATA, RANDOM_GROUPS, IMAGE);
	-- observes NAXIS NAXIS1 keys
	procedure Parse 
		(Cards : Card_Arr;
		 Prim  : out Standard_Primary_Type);

	-- Primary IMAGE

         type Primary_Image_Type is
                record
                        BITPIX : Integer;
                        NAXIS  : NAXIS_Type;
                        NAXISn : NAXIS_Arr(NAXIS_Range);
			-- FIXME for mem usage optimization provad NAXIS arr max length by generic
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
                        NAXIS  : NAXIS_Type;
                        NAXISn : NAXIS_Arr(NAXIS_Range);
			PCOUNT : Natural;
			GCOUNT : Positive;
                end record;

        procedure Parse
                (Cards : Card_Arr;
                 Keys  : in out Random_Groups_Type) is null;

	-- RandomGroups not supported -> no Compose()
	-- we need Parse only to determine DU-size 
	-- to be able to read other HDU's after RandGroups if present in the same file
	
       type Conforming_Extension_Type is
                record
                        BITPIX : Integer;
                        NAXIS  : NAXIS_Type;
                        NAXISn : NAXIS_Arr(NAXIS_Range);
			PCOUNT : Natural;
			GCOUNT : Positive;
                end record;

        procedure Parse
                (Cards : Card_Arr;
                 Keys  : in out Conforming_Extension_Type) is null;





end FITSlib.Header;
