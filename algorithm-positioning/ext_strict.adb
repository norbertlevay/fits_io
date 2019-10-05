


package body Ext_Strict is


	

RefKeys : array (Positive range 1..9) of String(1..8) := (
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

EmptyVal : constant String(1..20) := (others => ' ');	
type CardValue is
        record
                Value : String(1..20);
                Read  : Boolean;
        end record;
InitVal  : constant CardValue := (EmptyVal,False);

Vals : array (Positive range 1..RefKeys'Last) of CardValue;

NAXISn : array (Positive range 1.. NAXIS_Last) of CardValue;
TFORMn : array (Positive range 1.. NAXIS_Last) of CardValue;
TBCOLn : array (Positive range 1.. NAXIS_Last) of CardValue;

gCardsCount : Positive;
gENDCardSet : Boolean := False;

type State_Type is (INITIALIZED, SPECIAL_RECORDS, 
	CONFORMING_EXTENSION,	COLLECT_NAXIS_ARRAY, 
	COLLECT_TFORM_ARRAY, COLLECT_TBCOL_ARRAY,
	WAIT_END);

type State_Rec is 
	record
		State : State_Type;
		XTENSION : String(1..20);--value
		--Arr_Root : String;
		--Arr_Last : Positive;
		--Offset   : Natural;
	end record;

StateRec : State_Rec := (State => INITIALIZED, XTENSION => EmptyVal);


	procedure Reset_State 
	is
	begin
		StateRec.State  := INITIALIZED;
		StateRec.XTENSION  := EmptyVal;
		--StateRec.Offset := 0;
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

		if( RefKeys(RefPos) = String(Card(1..8)) ) then
			Vals(RefPos).Value := String(Card(11..30));
			Vals(RefPos).Read  := True;
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

		--case(RefPos) is -- FIXME needed ? these funcs are setting Pos
				-- in strict mode we force sequential card reading:
				-- if correctly implemented, it cannot happen that
				-- this func would be called for other then 2..3 5..7
		--	when 2 .. 3 | 5 .. 7 =>

				if ( RefKeys(RefPos) = String(Card(1..8)) )
				then
					Vals(RefPos).Value := String(Card(11..30));
					Vals(RefPos).Read  := True;
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
				     --  case(StateRec.ExtType) is
					--        when IMAGE =>
					--	       	StateRec.State := WAIT_END;
					  --      when TABLE | BINTABLE =>
					--	       	StateRec.State := COLLECT_TFORM_ARRAY;
						--others =>
							-- ERROR non standard Conforming Extension
						--	null;
				       -- end case;
					if(Vals(1).Value = "'IMAGE   '") then
						StateRec.State := WAIT_END;
					else
						StateRec.State := COLLECT_TFORM_ARRAY;
					end if;

									
				end if;

			--others =>
				-- ERROR programming error - should not happen
			--null;
		--end case;

		return RefPos + 1;-- FIXME not used

	end In_CONFORMING_EXTENSION;
	




	function Extract_Index(Root : String; CardKey : String) return Positive
	is
		RootLen : Positive := Root'Length;
	begin
		return Positive'Value( CardKey(RootLen .. 8) );
	end Extract_Index;



	function In_COLLECT_NAXIS_ARRAY(RefPos : Positive; Card : Card_Type) return Positive
	is
		Ix : Positive := Extract_Index("NAXIS",String(Card(1..8)));
		LenCardPos : constant Positive := 3;-- NAXIS card pos
		ArrLen     : constant Positive := Positive'Value(Vals(LenCardPos).Value);-- NAXISn arr length
	begin
		-- check root-name and position
		if ( ("NAXIS" = Card(1..5)) AND 
		     ((RefPos - LenCardPos) = Ix) )
		then
			NAXISn(Ix).Value := String(Card(11..30));
			NAXISn(Ix).Read  := True;
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
		Ix : Positive := Extract_Index("TFORM",String(Card(1..8)));
		LenCardPos : constant Positive := 7;-- TFIELD card pos
		ArrLen     : constant Positive := Positive'Value(Vals(LenCardPos).Value);-- TFORMn arr length
	begin
		-- check root-name and position
		if ( ("TFORM" = Card(1..5)) AND 
		     ((RefPos - NAXISCardPos - NAXISLength) = Ix) )
		then
			TFORMn(Ix) := String(Card(11..30));
		else
			-- ERROR unexpected card
			null;
		end if;

		-- if last array card read, change state
		if(Ix = ArrLen) 
		then
			case(StateRec.ExtType) is
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
		ArrLen     : constant Postive := Positive'Value(Vals(LenCardPos).Value);-- TBCOLn arr length
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
	




	function In_WAIT_END(RefPos : Positive; Card : Card_Type) return Natural
	is
	begin
		if( ENDCard = Card ) then
			gCardsCount := Pos;
			gENDCardSet := True;
			return 0;
		else
			return 1;
		end if;
	end In_WAIT_END;





	function Next (Pos : Positive;
		Card : Card_Type) return Positive
	is
		RefPos : Positive;
		rc : Natural := 1; -- continue
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
				rc := In_WAIT_END(RefPos, Card);
		end case;
		
		-- ask for next card
		return rc;

	end Next;




        --
        -- Interface : read by blocks
        --
        function  Next
                (BlockNum  : in Positive;
                 CardBlock : in Card_Block) return Read_Control
        is
		rr : Natural;
                Rc : Read_Control;
                CardPosBase : Natural := (BlockNum-1) * 36;
                CardPos : Positive;
                Card : Card_Type;
        begin
                for I in CardBlock'Range
                loop
                        Card := CardBlock(I);

                        if ( Card = ENDCard OR Value.Is_ValuedCard(Card) ) then

                                CardPos := CardPosBase + I;
                                rr := Next(CardPos, Card);
				
				if(rr = 0)
				then
					Rc := Stop
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
        begin
                if(StateRec.XTENSION = "'IMAGE   '") then
                                return EXT_IMAGE;
                elsif(StateRec.XTENSION = "'TABLE   '") then
                                return EXT_ASCII_TABLE;

                elsif(StateRec.XTENSION = "'BINTABLE'") then
                                return EXT_BIN_TABLE;
		end if;
		-- FIXME RAND_BLOCKS ?

        end To_HDU_Type;



        function  Get return HDU_Size_Info_Type
        is
                HDUSizeInfo : HDU_Size_Info_Type;
                NAXIS : Positive;
        begin
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


