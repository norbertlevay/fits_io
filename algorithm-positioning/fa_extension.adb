with Ada.Text_IO; use Ada.Text_IO; -- for debug only DBG_Print, Trace_State

with Ada.Exceptions; use Ada.Exceptions;

with FITS; use FITS; -- Card_Type needed
with Keyword_Record; use Keyword_Record;



package body FA_Extension is

EmptyVal : constant String(1..20) := (others => ' ');

	--
        -- definition of states
        --

type State_Name is
        (NOT_ACCEPTING_CARDS,  -- FA inactive
         CONFORMING_EXTENSION, -- Initial state: collect scalar card-values
         COLLECT_TABLE_ARRAYS, -- collect TFORM & TBCOL arrays and END-card
         WAIT_END,             -- ignore all cards except END-card
         IMAGE, TABLE, BINTABLE,	-- Final states
	 IMAGE_U, TABLE_U, BINTABLE_U, 	-- Final states
	 SPECIAL_RECORDS		-- Final states
	 );	
-- Final states naming: 
-- IMAGE : header contains exactly only mandatory cards for IMAGE, no other cards.
-- IMAGE_U : header has mandatory IMAGE cards and
-- some extra cards Unknown to this software implementation.
-- Other codes will refer to Reserved key groups, like B for biblio related keys:
-- IMAGE_BW : has only mandatory keys and at least one of biblio related 
-- reserved keys, and some WCS keys.


type CardValue is
        record
                Value : String(1..20);
                Read  : Boolean;
        end record;

InitVal  : constant CardValue := (EmptyVal,False);

type NAXIS_Arr   is array (1..NAXIS_Max)   of CardValue;
type TFIELDS_Arr is array (1..TFIELDS_Max) of CardValue;


InitNAXISArrVal : constant NAXIS_Arr   := (others => InitVal);
InitTFORMArrVal : constant TFIELDS_Arr := (others => InitVal);
InitTBCOLArrVal : constant TFIELDS_Arr := (others => InitVal);


type XT_Type is
        (UNSPECIFIED, IMAGE, ASCII_TABLE, BIN_TABLE);

type State_Type is
        record
	PrevPos : Natural;

        Name         : State_Name;
        XTENSION_Val : XT_Type;
        NAXIS_Val    : Natural;
        TFIELDS_Val  : Natural;

        XTENSION : CardValue;
        BITPIX   : CardValue;
        NAXIS    : CardValue;
        NAXISn   : NAXIS_Arr;
        PCOUNT   : CardValue;
        GCOUNT   : CardValue;
        TFIELDS  : CardValue;
        TFORMn   : TFIELDS_Arr;
        TBCOLn   : TFIELDS_Arr;
        UnknownCount : Natural;

        ENDCardPos : Natural;
        ENDCardSet : Boolean;
        end record;

InitState : State_Type := 
	(0,
	NOT_ACCEPTING_CARDS, UNSPECIFIED, 0, 0, 
	
	InitVal,InitVal,InitVal,
        InitNAXISArrVal,
        InitVal,InitVal,InitVal,
        InitTFORMArrVal,
        InitTBCOLArrVal,
	0,
        0,False);

State : State_Type := InitState;
------------------------------------------------------------------
package TIO renames Ada.Text_IO;

procedure DBG_Print
is
begin
TIO.New_Line;
TIO.Put(Boolean'Image(State.XTENSION.Read) & " XTENSION ");
TIO.Put_Line(State.XTENSION.Value);
TIO.Put(Boolean'Image(State.BITPIX.Read) & " BITPIX ");
TIO.Put_Line(State.BITPIX.Value);
TIO.Put(Boolean'Image(State.NAXIS.Read) & " NAXIS ");
TIO.Put_Line(State.NAXIS.Value);
TIO.Put("NAXIS: ");
for I in State.NAXISn'Range
loop
 if(State.NAXISn(I).Read) then Put(Positive'Image(I) &":"& State.NAXISn(I).Value & " "); end if;
end loop;
New_Line;
TIO.Put(Boolean'Image(State.PCOUNT.Read) & " PCOUNT ");
TIO.Put_Line(State.PCOUNT.Value);
TIO.Put(Boolean'Image(State.GCOUNT.Read) & " GCOUNT ");
TIO.Put_Line(State.GCOUNT.Value);
TIO.Put(Boolean'Image(State.TFIELDS.Read) & " TFIELDS ");
TIO.Put_Line(State.TFIELDS.Value);
TIO.Put("TFORM: ");
for I in State.TFORMn'Range
loop
	if(State.TFORMn(I).Read) then Put(Positive'Image(I) &":"& State.TFORMn(I).Value & " "); end if;
end loop;
New_Line;
TIO.Put("TBCOL: ");
for I in State.TBCOLn'Range
loop
	if(State.TBCOLn(I).Read) then Put(Positive'Image(I) &":"& State.TBCOLn(I).Value & " "); end if;
end loop;
New_Line;
TIO.Put_Line(State_Name'Image(State.Name));
end DBG_Print;


function To_XT_Type(XTENSION_Value : in String) return XT_Type
is
	t : XT_Type;
begin
	if(XTENSION_Value    = "'IMAGE   '          ") then
		t := IMAGE;
				
        elsif(XTENSION_Value = "'TABLE   '          ") then
                t := ASCII_TABLE;

        elsif(XTENSION_Value = "'BINTABLE'          ") then
                t := BIN_TABLE;

	else
		Raise_Exception(Unexpected_Card_Value'Identity, "XTENSION: " & XTENSION_Value);
	end if;
		
	return t;

end To_XT_Type;
-- -----------------------------------------------------------




	function Reset_State return Positive
	is
	begin
		State      := InitState;
		State.Name := CONFORMING_EXTENSION;
		return 1; -- start FA from Header's 1st card	
	end Reset_State;




	function In_CONFORMING_EXTENSION(Pos : Positive; Card : Card_Type) return Positive
	is
		Idx : Positive;
	begin
		TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));
		if(Pos = 1)
		then
			-- [FITS 3.5] The first 8 bytes of the special records 
			-- must not contain the string “XTENSION”.
			if( "XTENSION" = Card(1..8) )
			then
				State.XTENSION.Value := Card(11..30);
				State.XTENSION.Read  := True;
				State.XTENSION_Val := To_XT_Type(State.XTENSION.Value);
			else
				State.Name := SPECIAL_RECORDS;
			end if;

		elsif    ( "BITPIX  " = Card(1..8) AND (Pos = 2) )
		then
			State.BITPIX.Value := Card(11..30);
			State.BITPIX.Read  := True;

		elsif ( "NAXIS   " = Card(1..8) AND (Pos = 3) )
		then
			State.NAXIS.Value := Card(11..30);
			State.NAXIS.Read  := True;

			State.NAXIS_Val      := To_Integer(State.NAXIS.Value);

		elsif ( Is_Array(Card,"NAXIS",1,NAXIS_Max,Idx) )
			-- FIXME NAXIS_Max should be NAXIS_Val but how to 
			-- guarantee it is read before used ? The same for Idx: nested if's !!
		then
			if(Pos = 3 + Idx)
			then
				State.NAXISn(Idx).Value := Card(11..30);
				State.NAXISn(Idx).Read  := True;
			else
				Raise_Exception(Unexpected_Card'Identity, Card);
			end if;
	
		elsif ( "PCOUNT  " = Card(1..8) AND (Pos = 3 + State.NAXIS_Val + 1))
		then
			State.PCOUNT.Value := Card(11..30);
			State.PCOUNT.Read  := True;

		elsif ( "GCOUNT  " = Card(1..8) AND (Pos = 3 + State.NAXIS_Val + 2))
		then
			State.GCOUNT.Value := Card(11..30);
			State.GCOUNT.Read  := True;

			case(State.XTENSION_Val) is
				when IMAGE => State.Name := WAIT_END;
				when others => null;
			end case;

		elsif ("TFIELDS " = Card(1..8) AND (Pos = 3 + State.NAXIS_Val + 3) )
		then
			State.TFIELDS.Value := Card(11..30);
			State.TFIELDS.Read  := True;

			State.TFIELDS_Val := To_Integer(State.TFIELDS.Value);

			case(State.XTENSION_Val) is
				when ASCII_TABLE | BIN_TABLE => State.Name := COLLECT_TABLE_ARRAYS;
				when others => null;
			end case;

		else
			Raise_Exception(Unexpected_Card'Identity, Card);
		end if;

		return Pos + 1;

	end In_CONFORMING_EXTENSION;




        function Is_Fixed_Position(Card : in Card_Type) return Boolean
        is
        begin
                -- FIXME to be implemented
                return False;
        end Is_Fixed_Position;


        function Is_Valid(Card : in Card_Type) return Boolean
        is
        begin
                -- FIXME to be implemented
                return True;
        end Is_Valid;
	
	
	function In_WAIT_END(Pos : Positive; Card : Card_Type) return Natural
	is
	begin
		if( ENDCard = Card ) then
			State.ENDCardPos := Pos;
			State.ENDCardSet := True;

			TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));
			
		       	if(State.UnknownCount = 0)
                        then
				State.Name := IMAGE;
			else
				State.Name := IMAGE_U;
			end if;
			return 0;
			-- no more cards


                elsif(Is_Fixed_Position(Card)) then
                        -- one of CONFORMING_EXTENSION cards: may appear only once in header
                        Raise_Exception(Duplicate_Card'Identity, Card);

                elsif(Is_Valid(Card)) then
                        -- defined by [FITS Appendix A] BNF syntax
                        State.UnknownCount := State.UnknownCount + 1;
                        -- valid but unknown to this FA-implementation

                else
                        Raise_Exception(Invalid_Card'Identity, Card);
                        -- found card which does not confirm [FITS Addendix A] BNF syntax
                        -- FIXME consider configurable whether to raise excpetion here or ignore
		end if;

		return Pos + 1;

	end In_WAIT_END;


	-- NOTE Ada2005 has package Ada.Assertions; and also 'pragma Assert()'
  	procedure Assert_Array_Complete(ArrName : String; Length : Positive; Arr : TFIELDS_Arr )
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


	function In_COLLECT_TABLE_ARRAYS(Pos : Positive; Card : Card_Type) return Natural
	is
		Ix : Positive := 1;
	begin
		if ( "TFORM" = Card(1..5) )
		then
			Ix := Extract_Index("TFORM",Card(1..8));
			if(NOT State.TFORMn(Ix).Read)
			then
				State.TFORMn(Ix).Value := Card(11..30);
				State.TFORMn(Ix).Read := True;
			else
				-- FIXME only duplicates with diff values raises exception
				-- duplicate with equal values: make configurable what to do...
				Raise_Exception(Duplicate_Card'Identity, Card);
			end if;
			
			TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));

		elsif ( "TBCOL" = Card(1..5) )
		then
			if(State.XTENSION_Val = ASCII_TABLE) 
			then
				Ix := Extract_Index("TBCOL",Card(1..8));
				if(NOT State.TBCOLn(Ix).Read)
				then
					State.TBCOLn(Ix).Value := Card(11..30);
					State.TBCOLn(Ix).Read  := True;
				else
	                                Raise_Exception(Duplicate_Card'Identity, Card);
                	        end if;

				TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));
			else
				Raise_Exception(Unexpected_Card'Identity, Card);
			end if;

		elsif( Card = ENDCard )
		then
			State.ENDCardPos := Pos;
			State.ENDCardSet := True;

			TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));

                        case(State.XTENSION_Val) is
                               	when ASCII_TABLE => 
  					Assert_Array_Complete("TFORM",State.TFIELDS_Val, State.TFORMn);
  					Assert_Array_Complete("TBCOL",State.TFIELDS_Val, State.TBCOLn);
				       	if(State.UnknownCount = 0)
                        		then
						State.Name := TABLE;
					else
						State.Name := TABLE_U;
					end if;
					return 0;
					-- no more cards

                               	when BIN_TABLE   => 
  					Assert_Array_Complete("TFORM",State.TFIELDS_Val, State.TFORMn);
				       	if(State.UnknownCount = 0)
                        		then
						State.Name := BINTABLE;
					else
						State.Name := BINTABLE_U;
					end if;
					return 0;
					-- no more cards

                               	when others => null;
			end case;


		elsif(Is_Fixed_Position(Card))
		then
                        Raise_Exception(Duplicate_Card'Identity, Card);
                        -- one of CONFORMING_EXTENSION cards: may appear only once in header

                elsif(Is_Valid(Card)) then
                        -- valid card defined by [FITS Appendix A] BNF syntax
                        State.UnknownCount := State.UnknownCount + 1;
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
		Card : Card_Type) return Natural
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

			when CONFORMING_EXTENSION =>
				NextCardPos := In_CONFORMING_EXTENSION(Pos, Card);
			when WAIT_END =>
				NextCardPos := In_WAIT_END(Pos, Card);
			when COLLECT_TABLE_ARRAYS =>
				NextCardPos := In_COLLECT_TABLE_ARRAYS(Pos, Card);

			when SPECIAL_RECORDS | 
				IMAGE | IMAGE_U | 
				TABLE | TABLE_U | 
				BINTABLE | BINTABLE_U =>
				NextCardPos := 0;
		end case;
		
		if(NextCardPos = 0) then DBG_Print; end if;

		return NextCardPos;

	end Next;




        function To_HDU_Type(StateName : in FA_Extension.State_Name) return HDU_Type
        is
                t : HDU_Type;
        begin
                case(StateName) is
                        when SPECIAL_RECORDS       => t := SPECIAL_RECORDS;
                        when IMAGE | 	IMAGE_U    => t := EXT_IMAGE;
                        when TABLE | 	TABLE_U    => t := EXT_ASCII_TABLE;
                        when BINTABLE | BINTABLE_U => t := EXT_BIN_TABLE;
                        when others =>
                                Raise_Exception(Programming_Error'Identity,
                                "Not all cards read. State "&
                                FA_Extension.State_Name'Image(StateName) );
                end case;

                return t;

        end To_HDU_Type;



	function  Get return HDU_Size_Rec
        is
                HDUSizeInfo : HDU_Size_Rec;
                NAXIS : Positive;
	begin

		-- unlogical: code could not reach this point if ENDcard would not be found...
		-- NOT True: user can call this func without reading cards first: Programming_Error
                if(State.ENDCardSet) then
                        HDUSizeInfo.CardsCount := State.ENDCardPos;
                else
                        Raise_Exception(Card_Not_Found'Identity, "END");
                end if;

		-- until here handle together (??)





        	--
                -- conversion HDU_Size_Rec
                --

                -- FIXME how about XTENSION value, shoule we check it was set ?

                HDUSizeInfo.HDUType := To_HDU_Type(State.Name);

                if(State.BITPIX.Read) then
                        HDUSizeInfo.BITPIX := To_Integer(State.BITPIX.Value);
                else
                        Raise_Exception(Card_Not_Found'Identity, "BITPIX");
                end if;

                if(State.NAXIS.Read) then
                        NAXIS := To_Integer(State.NAXIS.Value);
                else
                        Raise_Exception(Card_Not_Found'Identity, "NAXIS");
                end if;

                for I in 1 .. NAXIS
                loop
                        if(State.NAXISn(I).Read) then
                                HDUSizeInfo.NAXISArr(I) := To_Integer(State.NAXISn(I).Value);
                        else
                                Raise_Exception(Card_Not_Found'Identity, "NAXIS"&Integer'Image(I));
                        end if;

                end loop;

                -- FIXME dirty fix: should return NAXISArr only NAXIS-long
                for I in NAXIS+1 .. NAXIS_Max
                loop
                        HDUSizeInfo.NAXISArr(I) := 1;
                end loop;


                return HDUSizeInfo;
        end Get;


end FA_Extension;



