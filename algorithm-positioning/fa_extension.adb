
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

with Inited_Value; use Inited_Value;
with Keyword_Record;

--
-- Finite Automaton
--

package body FA_Extension is

package KW renames Keyword_Record;


type State_Name is
        (NOT_ACCEPTING_CARDS,  	-- FA inactive
         IS_CONFORMING, 	-- Initial state
         COLLECT_TABLE_ARRAYS, 	-- collect TFORM & TBCOL arrays and END-card
         WAIT_END,             	-- ignore all cards except END-card
	 CONFORMING_EXTENSION,	-- Final state (is XTENSION bun not Standard)
	 IMAGE, TABLE, BINTABLE -- Final states (Standard Ext)
	 );	

TFIELDS_Max : constant Positive := 100;
type TBCOL_MaxArr is array (1 .. TFIELDS_Max) of CardValue(20);
type TFORM_MaxArr is array (1 .. TFIELDS_Max) of CardValue(70);

InitNAXISArrVal : constant NAXIS_MaxArr := (others => InitVal);
InitTFORMArrVal : constant TFORM_MaxArr  := (others => InitVal70);
InitTBCOLArrVal : constant TBCOL_MaxArr  := (others => InitVal);


type XT_Type is
        (UNSPECIFIED, IMAGE, ASCII_TABLE, BIN_TABLE);

type State_Type is
        record
	PrevPos : Natural;

        Name         : State_Name;
        XTENSION_Val : XT_Type;
        NAXIS_Val    : Natural;
        TFIELDS_Val  : Natural;

	-- Mandatory
        XTENSION : CardValue(70);
        BITPIX   : CardValue(20);
        NAXIS    : CardValue(20);
        NAXISn   : NAXIS_MaxArr;-- arr of CardValue20
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
	NOT_ACCEPTING_CARDS, UNSPECIFIED, 0, 0, 
	
	InitVal70,InitVal,InitVal,
        InitNAXISArrVal,
        InitVal,InitVal,InitVal,
        InitTFORMArrVal,
        InitTBCOLArrVal,
	0,
        0,False);

State : State_Type := InitState;
	
procedure DBG_Print is separate;

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

--
-- state transitions
--




	function Reset_State return Positive
	is
	begin
		State      := InitState;
                State.Name := IS_CONFORMING;
		return 1; -- start FA from Header's 1st card	
	end Reset_State;




	function In_IS_CONFORMING(Pos : Positive; Card : KW.Card_Type) return Positive
	is
		Idx : Positive;
	begin
		if(Pos = 1)
		then
			if( KW.Match_Key("XTENSION", Card) )
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

		elsif ( KW.Match_Indexed_Key("NAXIS", Card) )
		then
			Idx := KW.Take_Index("NAXIS", Card);
			if(Pos = 3 + Idx)
			then
				Set(State.NAXISn(Idx), Card);
			else
				Raise_Exception(Unexpected_Card'Identity, Card);
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

	end In_IS_CONFORMING;




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
			
        		case(State.XTENSION_Val) is
				when IMAGE  => State.Name := IMAGE;
				when ASCII_TABLE | BIN_TABLE => null;
				-- FIXME ??? cannot be In_WAIT_END if XTENSION_Val *_TABLE !!!
				when others => State.Name := CONFORMING_EXTENSION;
			end case;
			return 0;
			-- no more cards


                elsif(Is_Fixed_Position(Card))
		then
                        -- one of IS_CONFORMING cards: may appear only once in header
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
					State.Name := TABLE;
					return 0;
					-- no more cards

                               	when BIN_TABLE   => 
  					Assert_Array_Complete("TFORM",State.TFIELDS_Val, State.TFORMn);
					State.Name := BINTABLE;
					return 0;
					-- no more cards

                               	when others => null;
			end case;


		elsif(Is_Fixed_Position(Card))
		then
                        Raise_Exception(Duplicate_Card'Identity, Card);
                        -- one of IS_CONFORMING cards: may appear only once in header

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

			when IS_CONFORMING =>
				NextCardPos := In_IS_CONFORMING(Pos, Card);
			when WAIT_END =>
				NextCardPos := In_WAIT_END(Pos, Card);
			when COLLECT_TABLE_ARRAYS =>
				NextCardPos := In_COLLECT_TABLE_ARRAYS(Pos, Card);

			when CONFORMING_EXTENSION 
			     | IMAGE | TABLE |BINTABLE =>
				NextCardPos := 0;
		end case;
		
		if(NextCardPos = 0) then DBG_Print; end if;

		return NextCardPos;

	end Next;






-- read Mandatory keys as record

        function To_Extension_HDU(StateName : in FA_Extension.State_Name) return Extension_HDU
        is
                t : Extension_HDU;
        begin
                case(StateName) is
                        when CONFORMING_EXTENSION => t := CONFORMING_EXTENSION;
                        when IMAGE		=> t := STANDARD_IMAGE;
                        when TABLE		=> t := STANDARD_TABLE;
                        when BINTABLE		=> t := STANDARD_BINTABLE;
                        when others =>
                                Raise_Exception(Programming_Error'Identity,
                                "Not all cards read. State "&
                                FA_Extension.State_Name'Image(StateName) );
                end case;

                return t;

        end To_Extension_HDU;





	function Get_TFORMn return TFORM_Arr
	is
		Arr : TFORM_Arr(1 .. State.TFIELDS_Val);-- FIXME use 'First Last !!!!
	begin
		for I in 1 .. State.TFIELDS_Val
		loop
			if( State.TFORMn(I).Read )
			then 
				Arr(I) := To_Unbounded_String(Keyword_Record.To_String(State.TFORMn(I).Value));
				Put_Line("DBG >"&Arr(I)&"<"); 
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
                HDUSizeInfo : Result_Rec(To_Extension_HDU(State.Name),
					State.NAXIS_Val,
					State.TFIELDS_Val);
                NAXIS : Positive;
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
                        HDUSizeInfo.CardsCount := State.ENDCardPos;
                else
                        Raise_Exception(Card_Not_Found'Identity,   
                                        "END card not found, not a valid FITS file.");
                end if;

        	if(State.BITPIX.Read)
		then
                	HDUSizeInfo.BITPIX := KW.To_Integer(State.BITPIX.Value);
	        else
        		Raise_Exception(Card_Not_Found'Identity, "BITPIX");
                end if;
		-- FIXME here? to check whether XTENSION type vs BITPIX NAXIS values match ?
		
		-- FIXME see FA_Primary : NAXIS.Read vs NAXIS_Val
	        if(State.NAXIS.Read) then
        		NAXIS := KW.To_Integer(State.NAXIS.Value);
               	else
                       	Raise_Exception(Card_Not_Found'Identity, "NAXIS");
	        end if;

       	        for I in 1 .. NAXIS -- FIXME use NAXIS_Val ??
               	loop
                       	if(State.NAXISn(I).Read) then
                               	HDUSizeInfo.NAXISArr(I) := KW.To_Integer(State.NAXISn(I).Value);
                        else
       	                        Raise_Exception(Card_Not_Found'Identity, "NAXIS"&Integer'Image(I));
               	        end if;
                end loop;

		-- FIXME here? to check whether XTENSION type vs P/GCOUNT values match ?

		if(State.PCOUNT.Read) then
       	                HDUSizeInfo.PCOUNT := KW.To_Integer(State.PCOUNT.Value);
       		else
                       	Raise_Exception(Card_Not_Found'Identity, "PCOUNT");
                end if;

		if(State.GCOUNT.Read) then
       	                HDUSizeInfo.GCOUNT := KW.To_Integer(State.GCOUNT.Value);
      		else
                       	Raise_Exception(Card_Not_Found'Identity, "GCOUNT");
                end if;


		case HDUSizeInfo.HDU is
		when STANDARD_TABLE | STANDARD_BINTABLE =>

			HDUSizeInfo.TFORMn := Get_TFORMn;

			case HDUSizeInfo.HDU is
			when STANDARD_TABLE =>
				HDUSizeInfo.TBCOLn := Get_TBCOLn;
			when others => null;
			end case;

		when others => null;
		end case;
 

                return HDUSizeInfo;
        end Get;










end FA_Extension;

