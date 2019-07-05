--
-- Convert between array-of-cards <-> ada-record-fields
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




package FITS.Header is
	
	subtype CardRange    is Integer range  1 .. 80;

	subtype NameRange    is Integer range  1 ..  8;
	subtype ValueRange   is Integer range 11 .. 30;
	subtype CommentRange is Integer range 33 .. 80;
 
	subtype Card_Type is String(CardRange);
	type    Card_Arr  is array (Positive range <>) of Card_Type;


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
	
	--subtype CardValue_Type is String(1 .. 20);

	NAXIS_Last : constant := 999;
	subtype NAXIS_Type is Natural range 0 .. NAXIS_Last;
	type    NAXIS_Arr  is array (1 .. NAXIS_Type'Last) of Natural;
	
	type DataSize_Type is
		record
			SIMPLE   : String(ValueRange);
			GROUPS   : String(ValueRange);
			XTENSION : String(ValueRange);
			BITPIX : Integer;
			NAXIS  : NAXIS_Type;
			NAXISn : NAXIS_Arr;
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


end FITS.Header;
