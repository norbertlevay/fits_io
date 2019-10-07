with Ada.Text_IO; use Ada.Text_IO;

with Value;
with Primary_Size_Info;
use  Primary_Size_Info;
-- Card_Block, Read_Control, NAXIS_Last &
-- HDU_Size_Info_Type needed


package body Ext_Strict is

EmptyVal : constant String(1..20) := (others => ' ');	
type CardValue is
        record
                Value : String(1..20);
                Read  : Boolean;
        end record;
InitVal  : constant CardValue := (EmptyVal,False);

-- 1..6: "XTENSION", "BITPIX  ","NAXIS   ","NAXISn  ","PCOUNT  ","GCOUNT  ",
-- 7..9: "TFIELDS ","TFORMn  ","TBCOLn  "
Vals : array (Positive range 1..9) of CardValue;

TFIELDS_Max : constant Positive := 100;

NAXISn : array (Positive range 1.. NAXIS_Last) of CardValue;
TFORMn : array (Positive range 1.. TFIELDS_Max) of CardValue;
TBCOLn : array (Positive range 1.. TFIELDS_Max) of CardValue;

NAXIS_Val : Natural := 0;
TFIELDS_Val : Natural := 0;

gCardsCount : Positive;
gENDCardSet : Boolean := False;

type State_Type is 
	(INITIALIZED, SPECIAL_RECORDS, 
	CONFORMING_EXTENSION,	COLLECT_NAXIS_ARRAY, 
	COLLECT_TABLE_ARRAYS,
	WAIT_END);

type State_Rec is 
	record
		State : State_Type;
		XTENSION : String(1..20);
	end record;

StateRec : State_Rec := (State => INITIALIZED, XTENSION => EmptyVal);
------------------------------------------------------------------
package TIO renames Ada.Text_IO;

procedure DBG_Print
is
begin
TIO.Put(Boolean'Image(Vals(1).Read) & " XTENSION ");
TIO.Put_Line(Vals(1).Value);
TIO.Put(Boolean'Image(Vals(2).Read) & " BITPIX ");
TIO.Put_Line(Vals(2).Value);
TIO.Put(Boolean'Image(Vals(3).Read) & " NAXIS ");
TIO.Put_Line(Vals(3).Value);
TIO.Put("NAXIS: ");
for I in NAXISn'Range
loop
-- TIO.Put(Boolean'Image(NAXISn(I).Read) & " NAXIS" & Integer'Image(I)&" ");
-- TIO.Put_Line(NAXISn(I).Value);
 if(NAXISn(I).Read) then Put(Positive'Image(I) &":"& NAXISn(I).Value & " "); end if;
end loop;
New_Line;
TIO.Put(Boolean'Image(Vals(5).Read) & " PCOUNT ");
TIO.Put_Line(Vals(5).Value);
TIO.Put(Boolean'Image(Vals(6).Read) & " GCOUNT ");
TIO.Put_Line(Vals(6).Value);
TIO.Put(Boolean'Image(Vals(7).Read) & " TFIELDS ");
TIO.Put_Line(Vals(7).Value);
TIO.Put("TFORM: ");
for I in TFORMn'Range
loop
-- TIO.Put(Boolean'Image(TFORMn(I).Read) & " TFORM" & Integer'Image(I)&" ");
-- TIO.Put_Line(TFORMn(I).Value);
	if(TFORMn(I).Read) then Put(Positive'Image(I) &":"& TFORMn(I).Value & " "); end if;
end loop;
New_Line;
TIO.Put("TBCOL: ");
for I in TBCOLn'Range
loop
-- TIO.Put(Boolean'Image(TFORMn(I).Read) & " TFORM" & Integer'Image(I)&" ");
-- TIO.Put_Line(TFORMn(I).Value);
	if(TBCOLn(I).Read) then Put(Positive'Image(I) &":"& TBCOLn(I).Value & " "); end if;
end loop;
New_Line;
--TIO.Put(Boolean'Image(MandVals.ENDCardSet) & " END ");
--TIO.Put_Line(Positive'Image(MandVals.ENDCardPos));
TIO.Put_Line(State_Type'Image(StateRec.State));
end DBG_Print;
-- -----------------------------------------------------------



	procedure Reset_State 
	is
	begin
		TIO.New_Line;
		StateRec.State  := INITIALIZED;
		StateRec.XTENSION  := EmptyVal;

		for I in Vals'Range loop
			Vals(I).Value := EmptyVal;
		end loop;

	end Reset_State;




	function In_INITIALIZED(Pos : Positive; Card : Card_Type) return Positive
	is
	begin
		-- [FITS 3.5] The first 8 bytes of the special records 
		-- must not contain the string “XTENSION”.

		if(Pos /= 1) then
			-- ERROR in INITED state we must read card at first position
			null;
		end if;

		if( "XTENSION" = String(Card(1..8)) )
		then
			Vals(1).Value := String(Card(11..30));
			Vals(1).Read  := True;
			StateRec.State := CONFORMING_EXTENSION;
		else
			StateRec.State := SPECIAL_RECORDS;
		end if;

		return Pos + 1;

	end In_INITIALIZED;










	function In_CONFORMING_EXTENSION(Pos : Positive; Card : Card_Type) return Positive
	is
	begin
		if    ( "BITPIX  " = String(Card(1..8)) AND (Pos = 2) )
		then
			Vals(2).Value := String(Card(11..30));
			Vals(2).Read  := True;

		elsif ( "NAXIS   " = String(Card(1..8)) AND (Pos = 3) )
		then
			Vals(3).Value := String(Card(11..30));
			Vals(3).Read  := True;

			NAXIS_Val      := Natural'Value(Vals(3).Value);
			StateRec.State := COLLECT_NAXIS_ARRAY;
	
		elsif ( "PCOUNT  " = String(Card(1..8)) AND (Pos = 3 + NAXIS_Val + 1))
		then
			Vals(5).Value := String(Card(11..30));
			Vals(5).Read  := True;

		elsif ( "GCOUNT  " = String(Card(1..8)) AND (Pos = 3 + NAXIS_Val + 2))
		then
			Vals(6).Value := String(Card(11..30));
			Vals(6).Read  := True;

			if( (Vals(1).Value = "'TABLE   '          ") OR
			    (Vals(1).Value = "'BINTABLE'          ") )
			then
				StateRec.State := COLLECT_TABLE_ARRAYS;
			else
				StateRec.State := WAIT_END;
			end if;
	
		else
			-- ERROR unexpected card
			null;
		end if;

		return Pos + 1;

	end In_CONFORMING_EXTENSION;
	




	function Extract_Index(Root : String; CardKey : String) return Positive
	is
		RootLen : Positive := Root'Length;
	begin
		return Positive'Value( CardKey(RootLen+1 .. 8) );
	end Extract_Index;



	function In_COLLECT_NAXIS_ARRAY(Pos : Positive; Card : Card_Type) return Positive
	is
		Ix : Positive := Extract_Index("NAXIS",String(Card(1..8)));
	begin
		-- check root-name and position
		if ( ("NAXIS" = Card(1..5)) AND 
		     (Pos = 3 + Ix) )
		then
			NAXISn(Ix).Value := String(Card(11..30));
			NAXISn(Ix).Read  := True;
		else
			-- ERROR unexpected card
			null;
		end if;

		-- if last array card read, change state
		-- and read PCOUNT GCOUNT
		if(Ix = NAXIS_Val) 
		then
			StateRec.State := CONFORMING_EXTENSION;
		end if;

		return Pos + 1;

	end In_COLLECT_NAXIS_ARRAY;
	
	
	
	
	
		
	function In_COLLECT_TABLE_ARRAYS(Pos : Positive; Card : Card_Type) return Positive
	is
		Ix : Positive := 1;
	begin
		if ("TFIELDS " = String(Card(1..8)) )
                then
                	Vals(7).Value := String(Card(11..30));
                        Vals(7).Read  := True;
			TFIELDS_Val := Natural'Value(Vals(7).Value);
                else
			-- ERROR unexpected card
			null;
		end if;

		-- check root-name
		if ( ("TFORM" = Card(1..5)) )
		then
			Ix := Extract_Index("TFORM",String(Card(1..8)));
			TFORMn(Ix).Value := String(Card(11..30));
			TFORMn(Ix).Read := True;
		else
			-- ERROR unexpected card
			null;
		end if;

		-- if ASCII TABLE
		if(Vals(1).Value = "'TABLE   '          ") 
		then
			if ( ("TBCOL" = Card(1..5)) )
			then
				Ix := Extract_Index("TBCOL",String(Card(1..8)));
				TBCOLn(Ix).Value := String(Card(11..30));
				TBCOLn(Ix).Read  := True;
			else
				-- ERROR unexpected card
				null;
			end if;
		end if;

		-- if last array card read, change state
		if(Ix = TFIELDS_Val) 
		then
			StateRec.State := WAIT_END;
		end if;

		return Pos + 1;

	end In_COLLECT_TABLE_ARRAYS;
	


	function In_WAIT_END(Pos : Positive; Card : Card_Type) return Natural
	is
	begin
		if( ENDCard = Card ) then
			gCardsCount := Pos;
			gENDCardSet := True;
			return 0; -- no more cards
		else
			return Pos + 1;
		end if;
	end In_WAIT_END;




	--
	-- FA interface
	--
	function Next
		(Pos : Positive;
		Card : Card_Type) return Natural
	is
		NextCardPos : Natural;
		InState : State_Type := StateRec.State;
	begin

		case(StateRec.State) is
			when INITIALIZED => 
				NextCardPos := In_INITIALIZED(Pos, Card);

			when SPECIAL_RECORDS =>
				NextCardPos := 0; -- no more cards

			when CONFORMING_EXTENSION =>
				NextCardPos := In_CONFORMING_EXTENSION(Pos, Card);

			when COLLECT_NAXIS_ARRAY =>
				NextCardPos := In_COLLECT_NAXIS_ARRAY(Pos, Card);

			when COLLECT_TABLE_ARRAYS =>
				NextCardPos := In_COLLECT_TABLE_ARRAYS(Pos, Card);

			when WAIT_END =>
				NextCardPos := In_WAIT_END(Pos, Card);
		end case;
		
--		Put_Line("STATE "& State_Type'Image(InState) &"->"& State_Type'Image(StateRec.State) & " in: " & String(Card));

		-- ask for next card from this position
		return NextCardPos;

	end Next;




        --
        -- read by blocks FIXME move to Set_HDU
        --
        function  Next
                (BlockNum  : in Positive;
                 CardBlock : in Card_Block) return Read_Control
        is
		NextCardPos : Natural;
		Rc : Read_Control := Continue;
                CardPosBase : Natural := (BlockNum-1) * 36;
                CardPos : Positive;
                Card : Card_Type;
        begin
                for I in CardBlock'Range
                loop
                        Card := CardBlock(I);

                        if ( Card = ENDCard OR Value.Is_ValuedCard(Card) ) 
			then

                                CardPos := CardPosBase + I;
			       	NextCardPos := Next(CardPos, Card);
				-- currently ignored - we loop throu anyway
				
				if(NextCardPos = 0)
				then
					Rc := Stop;
					DBG_Print;
					exit;
				else
					Rc := Continue;
				end if;

                        end if;

                end loop;
                return Rc;
        end Next;


	-- Get interface
	
-- type HDU_Type is
  --      (PRIMARY_WITHOUT_DATA, PRIMARY_IMAGE, RANDOM_GROUPS,
  --      EXT_IMAGE, EXT_ASCII_TABLE, EXT_BIN_TABLE, RANDOM_BLOCKS);


        function To_HDU_Type(StateRec : in State_Rec) return HDU_Type
        is
		t : HDU_Type;
        begin
                if(StateRec.XTENSION = "'IMAGE   '") then
                                t := EXT_IMAGE;
				
                elsif(StateRec.XTENSION = "'TABLE   '") then
                                t := EXT_ASCII_TABLE;

                elsif(StateRec.XTENSION = "'BINTABLE'") then
                                t := EXT_BIN_TABLE;
		end if;
		-- FIXME RAND_BLOCKS ?
		
		return t;

        end To_HDU_Type;



        function  Get return HDU_Size_Info_Type
        is
                HDUSizeInfo : HDU_Size_Info_Type;
                NAXIS : Positive;
        begin
--		DBG_Print;

                HDUSizeInfo.HDUType    := To_HDU_Type(StateRec);
                -- will raise exception if state not PRIMARY* or RAND Groups

                if(gENDCardSet) then
                        HDUSizeInfo.CardsCount := gCardsCount;
                else
                        null;
                        -- ERROR raise exception No END card found
                end if;

                if(Vals(2).Read) then
                        HDUSizeInfo.BITPIX := Integer'Value(Vals(2).Value);
                else
                        null;
                        -- ERROR raise exception No BITPIX card found
                end if;

                if(Vals(3).Read) then
                        NAXIS := Integer'Value(Vals(3).Value);
                else
                        null;
                        -- ERROR raise exception No NAXIS card found
                end if;

                for I in 1 .. NAXIS
                loop
                        if(NAXISn(I).Read) then
                                HDUSizeInfo.NAXISArr(I) := Positive'Value(NAXISn(I).Value);
                        else
                                null;
                                -- ERROR raise exception No NAXIS(I) card found
                        end if;

                end loop;

                -- FIXME dirty fix: should return NAXISArr only NAXIS-long
                for I in NAXIS+1 .. NAXIS_Last
                loop
                        HDUSizeInfo.NAXISArr(I) := 1;
                end loop;


                return HDUSizeInfo;
        end Get;





end Ext_Strict;


