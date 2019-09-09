
package body Primary_Size_Info is

type State_Type is (
	UNSPECIFIED,   -- ?? Ada-code default
	INITIALIZED,   -- Reset_State was called
	PRIMARY_NON_STANDARD, -- SIMPLE = F card found: can calculate Header size biut not DU size
	PRIMARY_STANDARD, -- SIMPLE = T card found
	PRIMARY_NO_DATA,  -- NAXIS = 0
	PRIMARY_IMAGE,    -- NAXIS1 > 0
	RANDOM_GROUPS     -- NAXIS1 = 0
	);

State : State_Type := UNSPECIFIED;




type CardValue is
        record
                Value : String(1..20);
                ReadFromHeader : Boolean;
        end record;

type Primary_Mandatory_Card_Values is
        record
        SIMPLE : CardValue;
        BITPIX : CardValue;
        NAXIS  : CardValue;
        NAXIS1 : CardValue;
        NAXISArr : NAXIS_Arr;
        PCOUNT : CardValue;
        GCOUNT : CardValue;
        end record;

PrimMandVals : Primary_Mandatory_Card_Values;




	procedure Reset_State 
	is
	begin
		-- clear other state-variables
		State := INITIALIZED;
	end Reset_State;




	--
	-- State Transition functions
	--

	-- Note: below we are not checking the cards position; Should we ?
	-- to detect situations like: NAXIS5 found before NAXIS 
	-- or BITPIX card not in Header


	procedure In_INITIALIZED
		(Pos  : in Positive;
		 Card : in Card_Type) 
	is
	begin
		if (Card = SIMPLE_F_Card) then
			State := PRIMARY_NON_STANDARD;

		elsif (Card = SIMPLE_T_Card) then
			State := PRIMARY_STANDARD;
		else
			-- ERROR: unexpected card, non standard or broken Header
			null;
		end if;

	end In_INITIALIZED;




	procedure In_PRIMARY_STANDARD
		(Pos  : in Positive;
		 Card : in Card_Type) 
	is
	begin
		if (Card.Key = BITPIX_Card.Key) then
			BITPIX_Card.Value := Card.Value;
			BITPIX_Card.ValueRead := True;
		
		elsif (Card = NAXIS_0_Card.Key) then
			NAXIS_Card.Value := Card.Value;
			NAXIS_Card.ValueRead := True;
			State := PRIMARY_NO_DATA;
	
		elsif (Card = NAXIS_NonZero_Card.Key) then
			NAXIS_Card.Value := Card.Value;
			NAXIS_Card.ValueRead := True;
	
		elsif (Card = NAXIS1_NonZero_Card) then
			NAXISArr(1).Value := Card.Value;
			NAXISArr(1).ValueRead := True;
			State := PRIMARY_IMAGE;

		elsif (Card = NAXIS1_0_Card) then
			NAXISArr(1).Value := Card.Value;
			NAXISArr(1).ValueRead := True;
			State := RANDOM_GROUPS;

		else
			-- ERROR: unexpected card, non standard or broken Header
			null;
		end if;
		
	end In_PRIMARY_STANDARD;



-- NAXISArr() in Lixar should be of max size BuildLimit := StandardLimit(=999)
	-- after grammer check and decode it will be turned into aray of actual length as in Header


	procedure In_PRIMARY_IMAGE
		(Pos  : in Positive;
		 Card : in Card_Type) 
	is
	begin
		if (Is_NAXIS_Arr(Card)) then
			Idx := Decode_Index(Card.Key,"NAXIS");
			NAXISArr(Idx).Value := Card.Value;
			NAXISArr(Idx).ValueRead := True;

		else
			-- ERROR: unexpected card, non standard or broken Header
			null;
		end if;

	end In_PRIMARY_IMAGE;





	procedure In_RANDOM_GROUPS
		(Pos  : in Positive;
		 Card : in Card_Type) 
	is
	begin
		if (Is_NAXIS_Arr(Card)) then
			Idx := Decode_Index(Card.Key,"NAXIS");
			NAXISArr(Idx).Value := Card.Value;
			NAXISArr(Idx).ValueRead := True;
				
		elsif (Card.Key = PCOUNT_Card.Key) then
			PCOUNT_Card.Value := Card.Value;
			PCOUNT_Card.ValueRead := True;

		elsif (Card.Key = GCOUNT_Card.Key) then
			GCOUNT_Card.Value := Card.Value;
			GCOUNT_Card.ValueRead := True;
		
		elsif (Card = GROUPS_Card) then
			GROUPS_Card.Value := Card.Value;
			GROUPS_Card.ValueRead := True;
		
		else
			-- ERROR: unexpected card, non standard or broken Header
			null;
		end if;

	end In_RANDOM_GROUPS;






	--
	-- Interface
	--

	function  Next
		(Pos  : in Positive; 
		 Card : in Card_Block) return Loop_Type
	is
		ENDCardFound : Boolean := False;
	begin
		if( (NOT(State = UNSPECIFIED)) 
		     AND (Card = ENDCard) ) then
			-- store END card's position and mark set
			ENDCardFound := True;
			State := UNSPECIFIED;
		end if;

		case(State) is
			when INITIALIZED =>
			       In_INITIALIZED  (Pos, Card);
			when PRIMARY_NON_STANDARD =>
				null;
			when PRIMARY_STANDARD => 
			       In_PRIMARY_STANDARD(Pos, Card);
			when PRIMARY_NO_DATA =>
				null;
			when PRIMARY_IMAGE => 
			       In_PRIMARY_IMAGE(Pos, Card);	
			when RANDOM_GROUPS => 
			       In_RANDOM_GROUPS(Pos, Card);	
			when others => 
				-- including UNSPECIFIED
				-- programming error: Reset_State 
				-- must be called before New_Card
				null;
		end case;
		
		return ENDCardFound;

	end Next;





	
	--
	-- Collect Results
	--
	
	-- This is not Lexar, rather grammar rules
	--
	-- it is here that we check whether all values were set and so results are valid
	-- also interpret here semantics, like: 
	--  * if NAXIS exist, also NAXIS1... array must exist
	--  * if one card of a group present, all other cards of that group-type must be present
	--  * recognize alternative calibration sets cccccA card set and cccccB card set
	--  That all data for SizeClaculation was Read/Set
	--  and all those data is valid/within the range
	function  Get return HDU_Size_Info_Type
	is
		Results : HDU_Size_Info_Type;
	begin
		return Results;
	end Get;


end Primary_Size_Info;


