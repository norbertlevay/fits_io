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

	-- determine HDU type:

	-- * Caller: 
	-- UNKNOWN & FileIndex is  at BeginOfFile -> not a FITS file
        -- UNKNOWN # FileIndex not at BeginOfFile -> unspecified data at end of file 
	--
        -- * Implement inside callee:
        -- read first 8 char: 
        -- NOT SIMPLE and NOT XTENSION --> UNKNOWN
        -- read value 20 char:
        -- if SIMPLE and value NOT T and NOT F --> error, exit
        -- if SIMPLE + F --> PRIM_NON_STANDARD
        -- if SIMPLE + T --> read NAXIS (if not found -> error exit)
        -- if SIMPLE + T + NAXIS =0 --> PRIM_NO_DATA
        -- if SIMPLE + T + NAXIS!=0 --> PRIM_IMAGE
        -- if PRIM_IMAGE read NAXIS1 (if not found -> error, exit)
        -- if SIMPLE + T + NAXIS!=0 + NAXIS1=0 --> RAND_GROUPS (how about GROUSP=T ?)
        -- if XTENSION + IMAGE -> EXT_IMAGE 
        -- if XTENSION + TABLE -> EXT_TABLE
        -- if XTENSION + BINTABLE -> EXT_BINTABLE
        -- if XTENSION and NOT(3 above) -> EXT_UNKNOWN
        -- return;
        --
        -- .HDU::Peek(Source) return HDU_Variant:
        -- -- read block
        -- -- call Parse(block) return HDU_Variant
        -- -- Set_Index(Cur_Index - Block'Size);
        -- -- return HDU_Variant
	
	package STR renames Ada.Strings;
	type Card_State is (NOT_FOUND, IS_ZERO, NOT_ZERO);

	function Is_CardValue_Zero(Cards : Card_Arr; Key : String) return Card_State
	is
		Card  : Card_Type;
		State : Card_State := NOT_FOUND;
	begin

		for I in Cards'Range
		loop
			Card := Cards(I);
			if(Key = STR.Fixed.Trim(Card(NameRange),STR.Both))
			then
				if ("0" = STR.Fixed.Trim(Card(ValueRange),STR.Both))
				then
					State := IS_ZERO;
				else
					State := NOT_ZERO;
				end if;
			end if;
		end loop;

		return State;

	end Is_CardValue_Zero;



	function Handle_SIMPLE_T (Cards : Card_Arr) return HDU_Variant
	is
		Var   : HDU_Variant;
		NAXIS : Card_State := Is_CardValue_Zero(Cards,"NAXIS");
	begin

		if (NAXIS = NOT_FOUND)
		then
			-- error, raise exception StandardViolation:
			-- SIMPLE=T must have NAXIS-key within 1st block
			null;

		elsif(NAXIS = IS_ZERO) then

			Var := PRIM_NO_DATA;

		elsif(NAXIS = NOT_ZERO) then
			
			-- is Primary IMAGE --> read NAXIS1
			declare
				NAXIS_1 : Card_State := Is_CardValue_Zero(Cards,"NAXIS1");
			begin
				if(NAXIS_1 = NOT_FOUND)
				then
					-- error, raise exception StandardViolation:
					-- SIMPLE=T with NAXIS not zero
					-- must have NAXIS1-key within 1st block
					null;
				elsif(NAXIS_1 = IS_ZERO)  then Var := RAND_GROUPS;
				elsif(NAXIS_1 = NOT_ZERO) then Var := PRIM_IMAGE;
				end if;
			end;

		end if;

		return Var;

	end Handle_SIMPLE_T;


	-- [FITS] Table C.1 all needed keywords reside with 1st block
	function Parse (Cards : Card_Arr) return HDU_Variant
	is
		Var       : HDU_Variant;
		FirstCard    : Card_Type := Cards(1);
		FirstCardKey : String := Ada.Strings.Fixed.Trim(FirstCard(NameRange),Ada.Strings.Both);
		Is_SIMPLE   : Boolean := (FirstCardKey = "SIMPLE");
		Is_XTENSION : Boolean := (FirstCardKey = "XTENSION");

	begin

		if(Is_XTENSION) then

			declare
				FirstCardValue : String := Ada.Strings.Fixed.Trim(
					FirstCard(ValueRange),Ada.Strings.Both);
				Is_IMAGE : Boolean    := (FirstCardValue = "IMAGE");
				Is_TABLE : Boolean    := (FirstCardValue = "TABLE");
				Is_BINTABLE : Boolean := (FirstCardValue = "BINTABLE");
			begin
				if (Is_IMAGE)       then Var := EXT_IMAGE;
				elsif (Is_TABLE)    then Var := EXT_TABLE;
				elsif (Is_BINTABLE) then Var := EXT_BINTABLE;
		                else
					Var := EXT_UNKNOWN;
				end if;
			end;


		elsif(Is_SIMPLE) then

			declare
				FirstCardValue : String := Ada.Strings.Fixed.Trim(
					FirstCard(ValueRange),Ada.Strings.Both);
				Is_T : Boolean    := (FirstCardValue = "T");
				Is_F : Boolean    := (FirstCardValue = "F");
			begin
				if(Is_F)    then Var := PRIM_NON_STANDARD;
				elsif(Is_T) then Var := Handle_SIMPLE_T(Cards);
				else
					Var := PRIM_UNKNOWN;
				end if;
			end;

		else
			-- CardKey not SIMPLE not XTENSION
			Var := UNKNOWN;
		end if;

		return Var;
	end Parse;


	
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
			Keys.NAXIS := Natural'Value(Card(ValueRange));

		elsif(FITSlib.Key.Match(CardKey, "NAXIS",(1,NAXIS_Last), Index))
		then
			Keys.NAXISn(Index) := Positive'Value(Card(ValueRange));
		end if;
	
	end ParseCard;
	


	-- FIXME replace these for-cycles with generic with ParseCard()
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
