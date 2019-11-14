
with Ada.Exceptions; use Ada.Exceptions;

with Inited_Value; use Inited_Value;
with Keyword_Record;


-- FIXME consider configurable how to react to duplicates (with the same card value)
-- FIXME some keys may appear only in Primery (EXTEND,BLOCKED...?) and MUST NOT appear in extensions: should we check in extensions that they are not there ? -> distiguish between Standard-complience verification (taskA) vs. ambiguity if diff value (taskB)

--
-- Finite Automaton
--

package body FA_Primary is

package KW renames Keyword_Record;


type State_Name is 
       (NOT_ACCEPTING_CARDS, -- FA inactive
        PRIMARY_STANDARD,    -- Initial state: collect scalar card-values
        DATA_NOT_IMAGE,      -- collect GROUPS PCOUNT GCOUNT and END-card
        WAIT_END,            -- ignore all cards except END-card
        NO_DATA, IMAGE, RANDOM_GROUPS -- Final states
        );

subtype CardValue is Inited_Value.CardValue(20);

RANDG_Max   : constant Positive := 100;
type RANDG_MaxArr   is array (1 .. RANDG_Max)   of CardValue;

InitNAXISArrVal : constant NAXIS_MaxArr := (others => InitVal);
InitRANDGArrVal : constant RANDG_MaxArr := (others => InitVal);


type State_Type is
        record
	PrevPos : Natural;

        Name       : State_Name;
        NAXIS_Val  : Natural;
        NAXIS1_Val : Natural;

	-- Mandatory
        SIMPLE : CardValue;
        BITPIX : CardValue;
        NAXIS  : CardValue;
        NAXISn : NAXIS_MaxArr;-- arr of CardValue
        PCOUNT : CardValue;
        GCOUNT : CardValue;
        GROUPS : CardValue;

	-- other cards not recognized by this FA	
	OtherCount : Natural;

        ENDCardPos : Natural;
        ENDCardSet : Boolean;
        end record;

InitState : State_Type := 
	(
	0,
	NOT_ACCEPTING_CARDS, 0, 0,

        InitVal,InitVal,InitVal,
        InitNAXISArrVal,
        InitVal,InitVal,InitVal,
	0,
        0,False);

State : State_Type := InitState;

procedure DBG_Print is separate;


--
-- state transitions
--

	function Reset_State return Positive
	is
	begin
		State := InitState;
		State.Name := PRIMARY_STANDARD;
		return 1; -- start FA from 1st card of HDU
	end Reset_State;



	function In_PRIMARY_STANDARD
		(Pos  : in Positive;
		 Card : in KW.Card_Type) return Natural
	is
		Idx : Positive;
	begin

		if ( KW.Match_Key("SIMPLE", Card) AND (Pos = 1))
		then
			Set(State.SIMPLE, Card); 
			
			-- SIMPLE = F 
			-- non-standard primary HDU: don't know what to do -> exit.	
			if(KW.To_Boolean(State.SIMPLE.Value) = False)
			then
				Raise_Exception(Unexpected_Card_Value'Identity, Card);
			end if;


		elsif ( KW.Match_Key("BITPIX", Card) AND (Pos = 2))
		then
			Set(State.BITPIX, Card); 
		
		elsif ( KW.Match_Key("NAXIS", Card) AND (Pos = 3))
		then
			Set(State.NAXIS, Card); 

			State.NAXIS_Val := KW.To_Integer(State.NAXIS.Value);
	
			if (State.NAXIS_Val = 0)
			then
				State.Name := WAIT_END;
			end if;

		elsif ( KW.Match_Key("NAXIS1", Card) AND (Pos = 4))
		then
			Set(State.NAXISn(1), Card); 
			
			State.NAXIS1_Val := KW.To_Integer(State.NAXISn(1).Value);
	
		elsif ( KW.Match_Indexed_Key("NAXIS", Card) )
		then
			Idx := KW.Take_Index("NAXIS", Card);
			if(Pos = 3 + Idx)
			then
				Set(State.NAXISn(Idx), Card); 
			else
				Raise_Exception(Unexpected_Card'Identity, Card);
			end if;
			
			if(Idx >= State.NAXIS_Val)
			then
				if (State.NAXIS1_Val = 0) then
					State.Name := DATA_NOT_IMAGE;
				else
					State.Name := WAIT_END;
				end if;
			end if;

		else
			Raise_Exception(Unexpected_Card'Identity, Card);
		end if;
		
		return Pos + 1;

	end In_PRIMARY_STANDARD;





	
	function Is_Fixed_Position(Card : in KW.Card_Type) return Boolean
	is
	begin
		-- FIXME to be implemented
		return False;
	end Is_Fixed_Position;


	function Is_Valid(Card : in KW.Card_Type) return Boolean
	is
	begin
		-- FIXME to be implemented
		-- later hook up here Reserved/optional key parsing
		return True;
	end Is_Valid;



        function In_WAIT_END(Pos : Positive; Card : KW.Card_Type) return Natural
        is
        begin
		if( KW.ENDCard = Card )
		then
                       	State.ENDCardPos := Pos;
                       	State.ENDCardSet := True;

			if( State.NAXIS_Val = 0 )
			then
				State.Name := NO_DATA;
			else  
				State.Name := IMAGE;
			end if;

                        return 0;
			-- no more cards

		elsif(Is_Fixed_Position(Card))
		then
			-- one of PRIMARY_STANDARD cards: may appear only once in header
			Raise_Exception(Duplicate_Card'Identity, Card);

		elsif(Is_Valid(Card))
		then
			-- defined by [FITS Appendix A] BNF syntax
			State.OtherCount := State.OtherCount + 1;
			-- valid but unknown to this FA-implementation

		else
			Raise_Exception(Invalid_Card'Identity, Card);
			-- found card which does not confirm [FITS Addendix A] BNF syntax
			-- FIXME consider configurable whether to raise excpetion here or ignore
                end if;
	
		return Pos + 1;

        end In_WAIT_END;






	procedure Assert_GROUPS_PCOUNT_GCOUNT_Found
	is
	begin
		if(NOT State.GROUPS.Read) 
		then 
			Raise_Exception(Card_Not_Found'Identity, "GROUPS not found.");
		end if;
		if(NOT State.PCOUNT.Read) 
		then 
			Raise_Exception(Card_Not_Found'Identity, "PCOUNT not found.");
		end if;
		if(NOT State.GCOUNT.Read) 
		then 
			Raise_Exception(Card_Not_Found'Identity, "GCOUNT not found.");
		end if;
	end Assert_GROUPS_PCOUNT_GCOUNT_Found;


	procedure Assert_GROUPS_T_Found(Value : in String)
	is
	begin
	-- GROUPS = F
		if(KW.To_Boolean(Value) = False)
		then
			Raise_Exception(Unexpected_Card_Value'Identity, "Key: GROUPS  Value: " & Value);
		end if;
	end Assert_GROUPS_T_Found;


	function In_DATA_NOT_IMAGE
		(Pos  : in Positive;
		 Card : in KW.Card_Type) return Natural
	is
	begin

		-- Mandatory keys

		if ( KW.Match_Key("GROUPS", Card) )
		then
			if (NOT State.GROUPS.Read)
			then
				Set(State.GROUPS, Card);
			else
                                -- FIXME only duplicates with diff values raises exception
                                -- duplicate with equal values: make configurable what to do...
                                Raise_Exception(Duplicate_Card'Identity, Card);
			end if;

			Assert_GROUPS_T_Found(State.GROUPS.Value);

		elsif ( KW.Match_Key("PCOUNT", Card) )
		then
			if (NOT State.PCOUNT.Read)
			then
				Set(State.PCOUNT, Card);
			else
                                Raise_Exception(Duplicate_Card'Identity, Card);
			end if;
			

		elsif ( KW.Match_Key("GCOUNT", Card) )
		then
			if (NOT State.GCOUNT.Read)
			then
				Set(State.GCOUNT, Card);
			else
                                Raise_Exception(Duplicate_Card'Identity, Card);
			end if;


		elsif (Card = KW.ENDCard)
		then
			State.ENDCardPos := Pos;
                        State.ENDCardSet := True;

			Assert_GROUPS_PCOUNT_GCOUNT_Found;
 
			State.Name := RANDOM_GROUPS;
                        return 0;
			-- no more cards


		elsif(Is_Fixed_Position(Card))
		then
			
			Raise_Exception(Duplicate_Card'Identity, Card);
			-- one of PRIMARY_STANDARD cards: may appear only once in header

		elsif(Is_Valid(Card))
		then
			-- valid card defined by [FITS Appendix A] BNF syntax
			State.OtherCount := State.OtherCount + 1;
			-- valid but unknown to this FA-implementation
		else
			Raise_Exception(Invalid_Card'Identity, Card);
			-- found card which does not confirm [FITS Addendix A] BNF syntax
		end if;
		
	
		return Pos + 1;

	end In_DATA_NOT_IMAGE;





	--
	-- FA interface
	--
	function  Next
		(Pos  : in Positive; 
		Card : in KW.Card_Type) return Natural
	is
		NextCardPos : Natural;
	begin
		-- this FA-algorithm requires that cards are sequentially
		-- this check also guarantees that Fixed Pos cards cannot be skipped
		if(Pos /= State.PrevPos + 1)
		then
	                 Raise_Exception(Programming_Error'Identity, 
			   "Card in position returned from previous Next()-call must be supplied."
			   &" However: "&Integer'Image(Pos) &" prev: "&Integer'Image(State.PrevPos));
		else
			State.PrevPos := Pos;
		end if;


		case(State.Name) is
			when NOT_ACCEPTING_CARDS =>
				NextCardPos := 0;

			when PRIMARY_STANDARD => 
				NextCardPos := In_PRIMARY_STANDARD(Pos, Card);
			when WAIT_END =>
				NextCardPos := In_WAIT_END(Pos, Card);
			when DATA_NOT_IMAGE => 
				NextCardPos := In_DATA_NOT_IMAGE(Pos, Card);

			when NO_DATA | IMAGE | RANDOM_GROUPS =>
				NextCardPos := 0;
		end case;
		
		if(NextCardPos = 0) then DBG_Print; end if;

		return NextCardPos;

	end Next;





	
-- read Mandatory keys as record

	function To_Primary_HDU(S : State_Name) return Primary_HDU
	is
	begin
		case(S) is
			when NO_DATA => return NO_DATA;
			when IMAGE   => return IMAGE;
			when RANDOM_GROUPS => return RANDOM_GROUPS;
			when others =>
				Raise_Exception(Programming_Error'Identity, 
					"Get called but header not read or only partially read.");
		end case;
	end To_Primary_HDU;








	function  Get return Result_Rec
	is
		Result : Result_Rec(To_Primary_HDU(State.Name), State.NAXIS_Val);
        begin

                if(State.ENDCardSet)
		then
                        Result.CardsCount := State.ENDCardPos;
                else
                        Raise_Exception(Card_Not_Found'Identity, 
					"END card not found, not a valid FITS file.");
                end if;

                Result.BITPIX := KW.To_Integer(State.BITPIX.Value);

		case(Result.HDU) is
			when IMAGE | RANDOM_GROUPS =>
	
        	       	for I in 1 .. State.NAXIS_Val
               		loop
                       		if(State.NAXISn(I).Read)
				then
                       			Result.NAXISArr(I) := KW.To_Integer(State.NAXISn(I).Value);
                       		else
                               		Raise_Exception(Card_Not_Found'Identity, "NAXIS"&Integer'Image(I));
                       		end if;
               		end loop;
			
			case Result.HDU is
			when RANDOM_GROUPS =>
				Result.PCOUNT := KW.To_Integer(State.PCOUNT.Value);
				Result.GCOUNT := KW.To_Integer(State.GCOUNT.Value);
			when others => null;
			end case;

		when others =>null;
		end case;

 		return Result;

        end Get;

end FA_Primary;


