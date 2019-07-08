--
-- Notes for implementation:
--
-- FIXME make sure Ada Character type [Ada?][GNAT?]
-- is of same size as FITS Standard [FITS?] header-character
-- NOTE When composing and writing Mandatory keys to File many rules
-- must be kept: FITS-standard defines order of keys etc...

with Ada.Strings.Fixed;

with FITSlib.Key;


package body FITSlib.Header is
	
	-- dummy implement
        procedure Parse_First_Block
                (Cards : Card_Arr;
                 What  : out What_File)
	is
	begin
		What := STANDARD_PRIMARY;
	end Parse_First_Block;

 	procedure Parse
                (Cards : Card_Arr;
                 Prim  : out Standard_Primary_Type)is
	begin
		Prim := IMAGE;
	end Parse;

	
	-- parsing
	
	procedure ParseCard
               (Card : Card_Type;
                Keys : in out Primary_Image_Type)
	is
		CardKey   : String := Ada.Strings.Fixed.Trim(Card(NameRange), Ada.Strings.Both);
		CardValue : String := Ada.Strings.Fixed.Trim(Card(ValueRange),Ada.Strings.Both);
		-- FIXME CardValue runs on EACH card, even those which not match on Name/Key
		Index     : Positive;
	begin
		-- FIXME number conversion might raise exception
	
		if(CardKey = "BITPIX")
		then
			Keys.BITPIX := Integer'Value(Card(ValueRange));
		
		elsif(CardKey = "NAXIS")
		then
			Keys.NAXIS := Integer'Value(Card(ValueRange));

		elsif(FITSlib.Key.Match(CardKey, "NAXIS",(1,NAXIS_Last), Index))
		then
			Keys.NAXISn(Index) := Positive'Value(Card(ValueRange));
		end if;
	
	end ParseCard;
	


        procedure Parse
                (Cards : Card_Arr;
                 Keys  : in out Primary_Image_Type)
	is
	begin
                for I in Cards'Range
                loop
                        ParseCard(Cards(I), Keys);
                end loop;
	end Parse;


	-- info for header size calculation
	
	procedure Parse
		(Cards : Card_Arr;
		 Keys  : in out HeaderSize_Type)
	is
	begin
		for I in Cards'Range
		loop
			Keys.ENDCardFound := (Cards(I) = ENDCard);
			Keys.CardCount    := Keys.CardCount + 1;
			exit when Keys.ENDCardFound;
		end loop;
	end Parse;
	
	
	
	-- keys for data-size calculation
	
	procedure ParseCard
               (Card : Card_Type;
                Keys : in out DataSize_Type)
	is
		CardKey   : String := Ada.Strings.Fixed.Trim(Card(NameRange), Ada.Strings.Both);
		CardValue : String := Ada.Strings.Fixed.Trim(Card(ValueRange),Ada.Strings.Both);
		-- FIXME CardValue runs on EACH card, even those which not match on Name/Key
		Index     : Positive;
	begin
		-- FIXME number conversion might raise exception
		if (CardKey = "SIMPLE")
		then
			Keys.SIMPLE := Card(ValueRange);

		elsif (CardKey = "XTENSION")
		then
			Keys.XTENSION := Card(ValueRange);
		
		elsif (CardKey = "GROUPS")
		then
			Keys.GROUPS := Card(ValueRange);
		
		elsif(CardKey = "BITPIX")
		then
			Keys.BITPIX := Integer'Value(Card(ValueRange));
		
		elsif(CardKey = "NAXIS")
		then
			Keys.NAXIS := Integer'Value(Card(ValueRange));

		elsif(FITSlib.Key.Match(CardKey, "NAXIS",(1,NAXIS_Last), Index))
		then
			Keys.NAXISn(Index) := Positive'Value(Card(ValueRange));

		elsif(CardKey = "PCOUNT")
		then
			Keys.PCOUNT := Natural'Value(Card(ValueRange));
		
		elsif(CardKey = "GCOUNT")
		then
			Keys.GCOUNT := Positive'Value(Card(ValueRange));
		
		end if;
	
	end ParseCard;
	
	
	procedure Parse
		(Cards : Card_Arr;
		 Keys  : in out DataSize_Type)
	is
	begin
		for I in Cards'Range
		loop
			ParseCard(Cards(I), Keys);
		end loop;
	end Parse;
	
	
	procedure Compose
                (Keys  : DataSize_Type;
                 Cards : Card_Arr)
	is
	begin
		null;
	end Compose;



end FITSlib.Header;	
