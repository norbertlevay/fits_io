


package body Ext_Strict is


	

RefKeys : array (Positive 1..<>) of String(1..8) := (
	"XTENSION", 
	"BITPIX  ",
	"NAXIS   ",
	"NAXIS   ",
	"PCOUNT  ",
	"GCOUNT  ",
	"TFIELDS ",
	"TFORM   ",
	"TBCOL   "
		);
	

Vals : array (Positive 1..RefKeys'Last) of String(1..20);

NAXISn : array (Positive 1.. NAXIS_Last) of String(1..20);
TFORMn : array (Positive 1.. NAXIS_Last) of String(1..20);
TBCOLn : array (Positive 1.. NAXIS_Last) of String(1..20);


type State_Type is (INITIALIZED, SPECIAL_RECORDS, 
	CONFORMING_EXT,	COLLECT_NAXIS_ARRAY, 
	COLLECT_TFORM_ARRAY, COLLECT_TBCOL_ARRAY,
	WAIT_END);

type State_Rec is 
	record
		State : State_Type;
		XTENSION : String(1..20);--value
		Arr_Root : String;
		Arr_Last : Positive;
		Offset   : Natural;
	end record;

StateRec : State_Rec := (State => INITIALIZED, Offset => 0);


	procedure Reset_State 
	is
	begin
		StateRec.State  := INITIALIZED;
		StateRec.Offset := 0;
	end Reset_State;




	function In_INITIALIZED(RefPos : Positive; Card : Card_Type) return Positive
	is
	begin
		-- [FITS 3.5] The first 8 bytes of the special records 
		-- must not contain the string “XTENSION”.

		if(RefPos /= 1) then
			-- ERROR in INITED state we must read card at first position
			null;
		end if;

		if( RefKeys(RefPos) = Card(1..8) ) then
			Vals(RefPos)   := Card(11.30);
			StateRec.State := CONFORMING_EXTENSION;
		else
			StateRec.State := SPECIAL_RECORDS;
		end if;

		return RefPos + 1;-- FIXME not used -> return State instead(?)

	end In_INITIALIZED;







	function In_CONFORMING_EXTENSION(RefPos : Positive; Card : Card_Type) return Positive
	is
	begin
		-- cards BITPIX ... GCOUNT

		case(RefPos) is -- FIXME needed ? these funcs are setting Pos
				-- in strict mode we force sequential card reading:
				-- if correctly implemented, it cannot happen that
				-- this func would be called for other then 2..3 5..7
			when 2 .. 3, 5 .. 7 =>

				if ( RefKeys(RefPos) = Card(1..8) )
				then
					Vals(RefPos) := Card(11..30);
				else
					-- ERROR unexpected card
					null;
				end if;

				if(RefPos = 3) -- NAXIS
				then
					StateRec.State := COLLECT_NAXIS_ARRAY;
				end if;

				if(RefPos = 7) -- GCOUNT
				then
				       case(StateRec.ExtType)
					        when IMAGE =>
						       	StateRec.State := WAIT_END;
					        when TABLE, BINTABLE =>
						       	StateRec.State := COLLECT_TFORM_ARRAY;
						others =>
							-- ERROR non standard Conforming Extension
							null;
				       end case;
				end if;

			others =>
				-- ERROR programming error - should not happen
			null;
		end case;

		return RefPos + 1;-- FIXME not used

	end In_CONFORMING_EXTENSION;
	




	function In_COLLECT_NAXIS_ARRAY(RefPos : Positive; Card : Card_Type) return Positive
	is
		Ix : Positive := Extract_Index("NAXIS",Card(1..8));
		LenCardPos : constant Positive := 3;-- NAXIS card pos
		ArrLen     : constant Postive := Positive'Value(Vals(LenCardPos));-- NAXISn arr length
	begin
		-- check root-name and position
		if ( ("NAXIS" = Card(1..5)) AND 
		     ((RefPos - LenCardPos) = Ix) )
		then
			NAXISn(Ix) := Card(11..30);
		else
			-- ERROR unexpected card
			null;
		end if;

		-- if last array card read, change state
		-- return to read PCOUNT GCOUNT
		if(Ix = ArrLen) 
		then
			StateRec.State := CONFORMING_EXTENSION;
		end if;


		return RefPos + 1;-- FIXME not used

	end In_COLLECT_NAXIS_ARRAY;
	
	
	
	
	
		
	function In_COLLECT_TFORM_ARRAY(RefPos : Positive; Card : Card_Type) return Positive
	is
		Ix : Positive := Extract_Index("TFORM",Card(1..8));
		LenCardPos : constant Positive := 7;-- TFIELD card pos
		ArrLen     : constant Postive := Positive'Value(Vals(LenCardPos));-- TFORMn arr length
	begin
		-- check root-name and position
		if ( ("TFORM" = Card(1..5)) AND 
		     ((RefPos - NAXISCardPos - NAXISLength) = Ix) )
		then
			TFORMn(Ix) := Card(11..30);
		else
			-- ERROR unexpected card
			null;
		end if;

		-- if last array card read, change state
		if(Ix = ArrLen) 
		then
			case(StateRec.ExtType)
				when TABLE =>
					StateRec.State := WAIT_END;
				when BINTABLE =>
					StateRec.State := COLLECT_TBCOL_ARRAY;
			end case;
		end if;

		return RefPos + 1;-- FIXME not used

	end In_COLLECT_TFORM_ARRAY;
	






	function In_COLLECT_TBCOL_ARRAY(RefPos : Positive; Card : Card_Type) return Positive
	is
		Ix : Positive := Extract_Index("TBCOL",Card(1..8));
		LenCardPos : constant Positive := 7;-- TFIELD card pos
		ArrLen     : constant Postive := Positive'Value(Vals(LenCardPos));-- TBCOLn arr length
	begin
		-- check root-name and position
		if ( ("TBCOL" = Card(1..5)) AND 
		     ((RefPos - NAXISCardPos - NAXISLength - 3 - TFORMLength ) = Ix) )
		     -- 3 for PCOUNT GCOUNT TFIELDS
		then
			TBCOLn(Ix) := Card(11..30);
		else
			-- ERROR unexpected card
			null;
		end if;

		-- if last array card read, change state
		if(Ix = ArrLen) 
		then
			StateRec.State := WAIT_END;
		end if;

		return 1;-- FIXME not used

	end In_COLLECT_TBCOL_ARRAY;
	




	function In_WAIT_END(RefPos : Positive; Card : Card_Type) return Positive
	is
	begin
		if( ENDCard = Card ) then
			CardsCount := Pos;
		end if;

		return 1;-- FIXME not used

	end In_WAIT_END;





	function Next (Pos : Positive;
		Card : Card_Type) return Positive
	is
		RefPos : Positive;
	begin
		RefPos := Pos; -- offset not used:  - StateRec.Offset;

		case(StateRec.State) is
			when INITIALIZED => 
				In_INITIALIZED(RefPos, Card);

			when SPECIAL_RECORDS =>
				null;

			when CONFORMING_EXTENSION =>
				In_CONFORMING_EXTENSION(RefPos, Card);

			when COLLECT_NAXIS_ARRAY =>
				In_COLLECT_NAXIS_ARRAY(RefPos, Card);

			when COLLECT_TFORM_ARRAY =>
				In_COLLECT_TFORM_ARRAY(RefPos, Card);

			when COLLECT_TBCOL_ARRAY =>
				In_COLLECT_TBCOL_ARRAY(RefPos, Card);

			when WAIT_END =>
				In_WAIT_END(RefPos, Card);
		end case;
		
		-- ask for next card
		return Pos + 1;
	       -- FIXME how to tell card-reader to stop ? after ENDCard or SpecRecords found ??

	end Next;








end Ext_Strict;


