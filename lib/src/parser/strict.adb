
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

with Keyword_Record;

--
-- Finite Automaton
--

package body Strict is

package KW renames Keyword_Record;


type State_Name is
        (NOT_ACCEPTING_CARDS,  	-- FA inactive
         READ_FIXED_POSITION_CARDS, 	-- Initial state
         DATA_NOT_IMAGE, 	-- ignore all cards except GROUPS PCOUNT GCOUNT and END
         COLLECT_TABLE_ARRAYS, 	-- ignore all cards except TFORM & TBCOL arrays and END
         WAIT_END,             	-- ignore all cards except END
	 NO_DATA, IMAGE, RANDOM_GROUPS,  -- Final states (Primary)
	 CONFORMING_EXTENSION,		 -- Final states (Extensions)
	 ST_IMAGE, ST_TABLE, ST_BINTABLE -- Final states (Extensions)
	 );	

type CardValue(Last : Positive) is
        record
                Value : String(1..Last);
                Read  : Boolean;
        end record;

EmptyVal : constant String(1..20) := (others => ' ');
InitVal  : constant CardValue := (20,EmptyVal,False);

EmptyVal70 : constant String(1..70) := (others => ' ');
InitVal70  : constant CardValue := (70,EmptyVal70,False);



NAXIS_Max : constant Positive := 9;
type NAXIS_MaxArr is array (1 .. NAXIS_Max) of CardValue(20);

TFIELDS_Max : constant Positive := 100;
type TBCOL_MaxArr is array (1 .. TFIELDS_Max) of CardValue(20);
type TFORM_MaxArr is array (1 .. TFIELDS_Max) of CardValue(70);

InitNAXISArrVal : constant NAXIS_MaxArr := (others => InitVal);
InitTFORMArrVal : constant TFORM_MaxArr := (others => InitVal70);
InitTBCOLArrVal : constant TBCOL_MaxArr := (others => InitVal);


type XT_Type is
        (UNSPECIFIED, IMAGE, ASCII_TABLE, BIN_TABLE);

type State_Type is
        record
	PrevPos : Natural;

        Name         	: State_Name;
        XTENSION_Val 	: XT_Type;
        NAXIS_Val    	: Natural;
        NAXIS1_Val 	: Natural;
        TFIELDS_Val  	: Natural;

	-- Mandatory
        SIMPLE   : CardValue(20);
        XTENSION : CardValue(70);
        BITPIX   : CardValue(20);
        NAXIS    : CardValue(20);
        NAXISn   : NAXIS_MaxArr;-- arr of CardValue20
	GROUPS   : CardValue(20); -- FIXME check if needed: not return by Get
        PCOUNT   : CardValue(20);
        GCOUNT   : CardValue(20);
        TFIELDS  : CardValue(20);
        TFORMn   : TFORM_MaxArr;-- arr of CardValue70
        TBCOLn   : TBCOL_MaxArr;-- arr of CardValue20
	
	-- other cards not recognized by this FA
        OtherCount : Natural;

        ENDCardPos : Natural;
        ENDCardSet : Boolean;
        end record;

InitState : State_Type := 
	(
	0,
	NOT_ACCEPTING_CARDS, UNSPECIFIED, 0, 0, 0, 
	
	InitVal, InitVal70, 
	InitVal, InitVal,
        InitNAXISArrVal,
        InitVal, InitVal, InitVal,
	InitVal,
        InitTFORMArrVal,
        InitTBCOLArrVal,
	0,
        0,False);

State : State_Type := InitState;
	
--procedure DBG_Print is separate;

function To_XT_Type(XTENSION_Value : in String) return XT_Type
is
	t : XT_Type;
begin
	if(XTENSION_Value    = "IMAGE")
	then
		t := IMAGE;
				
        elsif(XTENSION_Value = "TABLE")
	then
                t := ASCII_TABLE;

        elsif(XTENSION_Value = "BINTABLE")
	then
                t := BIN_TABLE;

	else
		Raise_Exception(Unexpected_Card_Value'Identity, "XTENSION: " & XTENSION_Value);
	end if;
		
	return t;

end To_XT_Type;



function Is_Primary return Boolean
is
begin
	return State.XTENSION_Val = UNSPECIFIED;
end Is_Primary;
-- FIXME what if SpecRecords ? review




        procedure Set (V : in out CardValue; Card : in KW.Card_Type)
        is
        begin
                V.Value := String(Card(11..(10+V.Value'Last)));
                V.Read  := True;

                -- Trace theoretical DFA-state: 
                -- any change in implementation State-record (which is done by Set)
                -- means a state-change of theoretical DFA
-- FIXME make subpackage Trace_State
--                Ada.Text_IO.Put(
  --                              Ada.Strings.Fixed.Trim(Card(1..8), Ada.Strings.Both)
    --                            &"("
      --                          &Ada.Strings.Fixed.Trim(Card(11..30), Ada.Strings.Both)
        --                        &") ");

        end Set;




--
-- state transitions
--




	function Reset_State return Positive
	is
	begin
		State      := InitState;
                State.Name := READ_FIXED_POSITION_CARDS;
		return 1; -- start FA from Header's 1st card	
	end Reset_State;




	function In_READ_FIXED_POSITION_CARDS(Pos : Positive; Card : KW.Card_Type) return Positive
	is
		Idx : Positive;
	begin
		if(Pos = 1)
		then
 	              	if ( KW.Match_Key("SIMPLE", Card) )
			then
                	        Set(State.SIMPLE, Card);

                        	-- SIMPLE = F 
                        	-- non-standard primary HDU: don't know what to do -> exit.     
                        	if(KW.To_Boolean(State.SIMPLE.Value) = False)
                        	then
                                	Raise_Exception(Unexpected_Card_Value'Identity, Card);
                        	end if;

			elsif( KW.Match_Key("XTENSION", Card) )
			then
				Set(State.XTENSION, Card);

				State.XTENSION_Val := To_XT_Type(KW.To_String(State.XTENSION.Value));
			else
				-- possibly Special Records:
				-- [FITS 3.5] The first 8 bytes of the special records 
				-- must not contain the string “XTENSION”.
				Raise_Exception(Unexpected_First_Card'Identity, Card);
			end if;

		elsif  ( KW.Match_Key("BITPIX", Card) AND (Pos = 2) )
		then
			Set(State.BITPIX, Card);

		elsif ( KW.Match_Key("NAXIS", Card) AND (Pos = 3) )
		then
			Set(State.NAXIS, Card);

			State.NAXIS_Val := KW.To_Integer(State.NAXIS.Value);

			-- from Primary

 			if (State.NAXIS_Val = 0)
                        then
                                State.Name := WAIT_END;
				-- FIXME what if Extension has NAXIS=0 ?? check standard
                        end if;


		elsif ( KW.Match_Indexed_Key("NAXIS", Card) )
		then
			Idx := KW.Take_Index("NAXIS", Card);

			if(Pos = 3 + Idx)
			then
				Set(State.NAXISn(Idx), Card);
			else
				Raise_Exception(Unexpected_Card'Identity, Card);
			end if;

			-- from Primary
			if(Is_Primary)
			then
	                        if ( Idx = 1 )
        	                then
                	                State.NAXIS1_Val := KW.To_Integer(State.NAXISn(1).Value);
                        	end if;
				-- FIXME what if NAXIS=1 NAXIS1=0 ??? check Standard
	                        if(Idx >= State.NAXIS_Val)
        	                then
                	                if (State.NAXIS1_Val = 0) then
                        	                State.Name := DATA_NOT_IMAGE;                                	else
                                        	State.Name := WAIT_END;
                                	end if;
                        	end if;

			end if;

	
		elsif ( KW.Match_Key("PCOUNT", Card) AND (Pos = 3 + State.NAXIS_Val + 1))
		then
			Set(State.PCOUNT, Card);

		elsif ( KW.Match_Key("GCOUNT", Card) AND (Pos = 3 + State.NAXIS_Val + 2))
		then
			Set(State.GCOUNT, Card);

			case(State.XTENSION_Val) is
				when IMAGE  => State.Name := WAIT_END;
				when ASCII_TABLE | BIN_TABLE => null;
				when others => State.Name := WAIT_END;
			end case;

		elsif ( KW.Match_Key("TFIELDS", Card) AND (Pos = 3 + State.NAXIS_Val + 3) )
		then
			Set(State.TFIELDS, Card);

			State.TFIELDS_Val := KW.To_Integer(State.TFIELDS.Value);

			case(State.XTENSION_Val) is
				when ASCII_TABLE | BIN_TABLE =>
					State.Name := COLLECT_TABLE_ARRAYS;
				when others => 
					Raise_Exception(Unexpected_Card'Identity, Card);
			end case;

		else
			Raise_Exception(Unexpected_Card'Identity, Card);
		end if;

		return Pos + 1;

	end In_READ_FIXED_POSITION_CARDS;




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
                -- later hook up here Reserved/Optional Key parsing
                return True;
        end Is_Valid;


	function In_WAIT_END(Pos : Positive; Card : KW.Card_Type) return Natural
	is
	begin
		if( KW.ENDCard = Card )
		then
			State.ENDCardPos := Pos;
			State.ENDCardSet := True;
		
			-- from Primary
			if(Is_Primary)
			then
	                      	if( State.NAXIS_Val = 0 )
        	                then
                	                State.Name := NO_DATA;
                        	else
                                	State.Name := IMAGE;
                        	end if;
			else
	        		case(State.XTENSION_Val) is
					when IMAGE  => State.Name := ST_IMAGE;
					when ASCII_TABLE | BIN_TABLE => null;
					-- FIXME  cannot be In_WAIT_END if XTENSION_Val *_TABLE !!!
					when others => State.Name := CONFORMING_EXTENSION;
				end case;
			end if;

			return 0;
			-- no more cards


                elsif(Is_Fixed_Position(Card))
		then
                        -- one of READ_FIXED_POSITION_CARDS cards: may appear only once in header
                        Raise_Exception(Duplicate_Card'Identity, Card);

                elsif(Is_Valid(Card))
		then
                        -- defined by [FITS Appendix A] BNF syntax
                        State.OtherCount := State.OtherCount + 1;
                        -- valid but unknown to this FA-implementation

                else
                        Raise_Exception(Invalid_Card'Identity, Card);
                        -- found card which does not conform [FITS Addendix A] BNF syntax
                        -- FIXME consider configurable whether to raise excpetion here or ignore
		end if;

		return Pos + 1;

	end In_WAIT_END;

	-- from Primary
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
                        Raise_Exception(Unexpected_Card_Value'Identity, 
						"Key: GROUPS  Value: " & Value);
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



	-- NOTE Ada2005 has package Ada.Assertions; and also 'pragma Assert()'
   	procedure Assert_Array_Complete(ArrName : String; Length : Positive; Arr : TBCOL_MaxArr )
	is
		ArrComplete : Boolean := True;
	begin
		for Ix in 1 .. Length 
		loop
			if(Arr(Ix).Read)
			then
				null;
			else
				Raise_Exception(Card_Not_Found'Identity, ArrName&Integer'Image(Ix));
			end if;
		end loop;
	end Assert_Array_Complete;

 	procedure Assert_Array_Complete(ArrName : String; Length : Positive; Arr : TFORM_MaxArr )
	is
		ArrComplete : Boolean := True;
	begin
		for Ix in 1 .. Length 
		loop
			if(Arr(Ix).Read)
			then
				null;
			else
				Raise_Exception(Card_Not_Found'Identity, ArrName&Integer'Image(Ix));
			end if;
		end loop;
	end Assert_Array_Complete;




	function In_COLLECT_TABLE_ARRAYS(Pos : Positive; Card : KW.Card_Type) return Natural
	is
		Idx : Positive := 1;
	begin

		if ( KW.Match_Indexed_Key("TFORM", Card) )
		then
			Idx := KW.Take_Index("TFORM", Card);
			if(NOT State.TFORMn(Idx).Read)
			then
				Set(State.TFORMn(Idx), Card);
			else
				-- FIXME only duplicates with diff values raises exception
				-- duplicate with equal values: make configurable what to do...
				Raise_Exception(Duplicate_Card'Identity, Card);
			end if;
			

		elsif ( KW.Match_Indexed_Key("TBCOL", Card) )
		then
			if(State.XTENSION_Val = ASCII_TABLE) 
			then
				Idx := KW.Take_Index("TBCOL", Card);
				if(NOT State.TBCOLn(Idx).Read)
				then
					Set(State.TBCOLn(Idx), Card);
				else
	                                Raise_Exception(Duplicate_Card'Identity, Card);
                	        end if;

			else
				Raise_Exception(Unexpected_Card'Identity, Card);
			end if;


		elsif( Card = KW.ENDCard )
		then
			State.ENDCardPos := Pos;
			State.ENDCardSet := True;

                        case(State.XTENSION_Val) is
                               	when ASCII_TABLE => 
  					Assert_Array_Complete("TFORM",State.TFIELDS_Val, State.TFORMn);
  					Assert_Array_Complete("TBCOL",State.TFIELDS_Val, State.TBCOLn);
					State.Name := ST_TABLE;
					return 0;
					-- no more cards

                               	when BIN_TABLE   => 
  					Assert_Array_Complete("TFORM",State.TFIELDS_Val, State.TFORMn);
					State.Name := ST_BINTABLE;
					return 0;
					-- no more cards

                               	when others => null;
			end case;


		elsif(Is_Fixed_Position(Card))
		then
                        Raise_Exception(Duplicate_Card'Identity, Card);
                        -- one of READ_FIXED_POSITION_CARDS cards: may appear only once in header

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

	end In_COLLECT_TABLE_ARRAYS;
	





	--
	-- FA interface
	--
	function Next
		(Pos : Positive;
		Card : KW.Card_Type) return Natural
	is
		NextCardPos : Natural;
		InState : State_Name := State.Name;
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

			when READ_FIXED_POSITION_CARDS =>
				NextCardPos := In_READ_FIXED_POSITION_CARDS(Pos, Card);
			when WAIT_END =>
				NextCardPos := In_WAIT_END(Pos, Card);
                        when DATA_NOT_IMAGE =>
                                NextCardPos := In_DATA_NOT_IMAGE(Pos, Card);
			when COLLECT_TABLE_ARRAYS =>
				NextCardPos := In_COLLECT_TABLE_ARRAYS(Pos, Card);

			when NO_DATA | IMAGE | RANDOM_GROUPS
			     | CONFORMING_EXTENSION 
			     | ST_IMAGE | ST_TABLE | ST_BINTABLE =>
				NextCardPos := 0;
		end case;
		
--		if(NextCardPos = 0) then DBG_Print; end if;

		return NextCardPos;

	end Next;






-- read Mandatory keys as record


        function To_HDU_Type(StateName : in State_Name) return HDU_Type
        is
                t : HDU_Type;
        begin
                case(StateName) is
                        when NO_DATA => return NO_DATA;
                        when IMAGE   => return IMAGE;
                        when RANDOM_GROUPS => return RANDOM_GROUPS;

                        when CONFORMING_EXTENSION => t := CONFORMING_EXTENSION;
                        when ST_IMAGE		=> t := STANDARD_IMAGE;
                        when ST_TABLE		=> t := STANDARD_TABLE;
                        when ST_BINTABLE	=> t := STANDARD_BINTABLE;
                        when others =>
                                Raise_Exception(Programming_Error'Identity,
                                "Not all cards read. State "&
                                State_Name'Image(StateName) );
                end case;

                return t;

        end To_HDU_Type;





	function Get_TFORMn return TFORM_Arr
	is
		Arr : TFORM_Arr(1 .. State.TFIELDS_Val);-- FIXME use 'First Last !!!!
	begin
		for I in 1 .. State.TFIELDS_Val
		loop
			if( State.TFORMn(I).Read )
			then 
				Arr(I) := To_Unbounded_String(Keyword_Record.To_String(State.TFORMn(I).Value));
			else
				null; -- FIXME what if some value missing ?
			end if;
		end loop;
		return Arr;
	end Get_TFORMn;









	function Get_TBCOLn return KW.Positive_Arr
	is
		Arr : KW.Positive_Arr(1 .. State.TFIELDS_Val);-- FIXME use 'First Last !!!!
	begin
		for I in 1 .. State.TFIELDS_Val
		loop
			if( State.TBCOLn(I).Read )
			then 
				Arr(I) := KW.To_Integer(State.TBCOLn(I).Value); 
			else
				null; -- FIXME what if some value missing ?
			end if;
		end loop;
		return Arr;
	end Get_TBCOLn;




	function  Get return Result_Rec
        is
		-- FIXME how about XTENSION value, shoule we check it was set ?
                Result : Result_Rec(To_HDU_Type(State.Name),
					State.NAXIS_Val,
					State.TFIELDS_Val);
--                NAXIS : Positive;
	begin
                if(State.OtherCount > 0)
                then
			null;
                end if;
                -- NOTE: user can simply call Get() without running the FA -> programming error
                -- OR Header was read, but is broken (without END card) so we read through all file
                -- reaching EOF <- but this should raise exception: trying to read behind file end
                -- OR user called FA on non FITS file and so probably scans through until EOF


                if(State.ENDCardSet) then
                        Result.CardsCount := State.ENDCardPos;
                else
                        Raise_Exception(Card_Not_Found'Identity,   
                                        "END card not found, not a valid FITS file.");
                end if;

        	if(State.BITPIX.Read)
		then
                	Result.BITPIX := KW.To_Integer(State.BITPIX.Value);
	        else
        		Raise_Exception(Card_Not_Found'Identity, "BITPIX");
                end if;
		-- FIXME here? to check whether XTENSION type vs BITPIX NAXIS values match ?
		
		-- FIXME here? to check whether XTENSION type vs P/GCOUNT values match ?

		case Result.HDU is
		when IMAGE .. STANDARD_BINTABLE  =>

	        	if(State.NAXIS.Read)
			then
	       	        	for I in 1 .. State.NAXIS_Val
        	       		loop
                	       		if(State.NAXISn(I).Read) then
                        	       		Result.NAXISArr(I) := 
							KW.To_Integer(State.NAXISn(I).Value);
                       			else
       	                        		Raise_Exception(Card_Not_Found'Identity, 
								"NAXIS"&Integer'Image(I));
               	        		end if;
                		end loop;
			end if;

			case Result.HDU is
			when RANDOM_GROUPS .. STANDARD_BINTABLE =>

				if(State.PCOUNT.Read) then
       	        	        	Result.PCOUNT := KW.To_Integer(State.PCOUNT.Value);
       				else
                       			Raise_Exception(Card_Not_Found'Identity, "PCOUNT");
                		end if;

				if(State.GCOUNT.Read) then
       	                		Result.GCOUNT := KW.To_Integer(State.GCOUNT.Value);
      				else
                       			Raise_Exception(Card_Not_Found'Identity, "GCOUNT");
                		end if;

				case Result.HDU is
				when STANDARD_TABLE | STANDARD_BINTABLE =>

					Result.TFORMn := Get_TFORMn;

					case Result.HDU is
					when STANDARD_TABLE =>
						Result.TBCOLn := Get_TBCOLn;
					when others => null;
					end case;

				when others => null;
				end case;

			when others => null;
			end case;

		when others => null;
		end case;
 

                return Result;
        end Get;










end Strict;

