--
-- Notes for implementation:
--
-- FIXME make sure Ada Character type [Ada?][GNAT?]
-- is of same size as FITS Standard [FITS?] header-character
-- NOTE When composing and writing Mandatory keys to File many rules
-- must be kept: FITS-standard defines order of keys etc...


package body FITS.Header is
	
	
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
		CardKey : String := Card(1..8); -- Trim FIXME
	begin
		if (CardKey = "SIMPLE")
		then
			Keys.SIMPLE := Card(11..30);

		elsif (CardKey = "XTENSION")
		then
			Keys.XTENSION := Card(11..30);
		
		elsif (CardKey = "GROUPS")
		then
			Keys.GROUPS := Card(11..30);
		
		elsif(CardKey = "BITPIX")
		then
			Keys.BITPIX := 1;
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



end FITS.Header;	
