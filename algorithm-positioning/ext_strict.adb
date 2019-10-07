with Ada.Text_IO; use Ada.Text_IO;

with FITS; use FITS;
-- Card_Type, NAXIS_Last, HDU_Size_Info_Type needed

with Value;



package body Ext_Strict is

EmptyVal : constant String(1..20) := (others => ' ');

type CardValue is
        record
                Value : String(1..20);
                Read  : Boolean;
        end record;
InitVal  : constant CardValue := (EmptyVal,False);

--
-- collected values
--
type NAXIS_Arr is array (1..NAXIS_Last) of CardValue;
type TFORM_Arr is array (1..TFIELDS_Max) of CardValue;
type TBCOL_Arr is array (1..TFIELDS_Max) of CardValue;
InitNAXISArrVal : constant NAXIS_Arr := (others => InitVal);
InitTFORMArrVal : constant TFORM_Arr := (others => InitVal);
InitTBCOLArrVal : constant TBCOL_Arr := (others => InitVal);

type Extension_Mandatory_Card_Values is
        record
        XTENSION : CardValue;
        BITPIX   : CardValue;
        NAXIS    : CardValue;
        NAXISn   : NAXIS_Arr;
        PCOUNT   : CardValue;
        GCOUNT   : CardValue;
        TFIELDS  : CardValue;
        TFORMn   : TFORM_Arr;
        TBCOLn   : TBCOL_Arr;
        ENDCardPos : Natural;
        ENDCardSet : Boolean;
        end record;

InitMandVals : Extension_Mandatory_Card_Values := (InitVal,InitVal,InitVal,
                                                 InitNAXISArrVal,
                                                 InitVal,InitVal,InitVal,
                                                 InitTFORMArrVal,
                                                 InitTBCOLArrVal,
                                                 0,False);

MandVals : Extension_Mandatory_Card_Values := InitMandVals;


--
-- state definition
--
type State_Name is 
	(INITIALIZED, RANDOM_BLOCKS, 
	CONFORMING_EXTENSION, COLLECT_NAXIS_ARRAY, 
	COLLECT_TABLE_ARRAYS,
	WAIT_END);

type State_Type is 
	record
		Name        : State_Name;
		XTENSION    : HDU_Type;
		NAXIS_Val   : Natural;
		TFIELDS_Val : Natural;
	end record;
-- collects values used in state-change decisions

InitState : State_Type := (Name => INITIALIZED, NAXIS_Val => 0, TFIELDS_Val => 0, XTENSION => UNSPECIFIED);
State : State_Type := InitState;

------------------------------------------------------------------
-- utils
--
package TIO renames Ada.Text_IO;

procedure DBG_Print
is
begin
TIO.Put(Boolean'Image(MandVals.XTENSION.Read) & " XTENSION ");
TIO.Put_Line(MandVals.XTENSION.Value);
TIO.Put(Boolean'Image(MandVals.BITPIX.Read) & " BITPIX ");
TIO.Put_Line(MandVals.BITPIX.Value);
TIO.Put(Boolean'Image(MandVals.NAXIS.Read) & " NAXIS ");
TIO.Put_Line(MandVals.NAXIS.Value);
TIO.Put("NAXIS: ");
for I in MandVals.NAXISn'Range
loop
-- TIO.Put(Boolean'Image(NAXISn(I).Read) & " NAXIS" & Integer'Image(I)&" ");
-- TIO.Put_Line(NAXISn(I).Value);
 if(MandVals.NAXISn(I).Read) then Put(Positive'Image(I) &":"& MandVals.NAXISn(I).Value & " "); end if;
end loop;
New_Line;
TIO.Put(Boolean'Image(MandVals.PCOUNT.Read) & " PCOUNT ");
TIO.Put_Line(MandVals.PCOUNT.Value);
TIO.Put(Boolean'Image(MandVals.GCOUNT.Read) & " GCOUNT ");
TIO.Put_Line(MandVals.GCOUNT.Value);
TIO.Put(Boolean'Image(MandVals.TFIELDS.Read) & " TFIELDS ");
TIO.Put_Line(MandVals.TFIELDS.Value);
TIO.Put("TFORM: ");
for I in MandVals.TFORMn'Range
loop
-- TIO.Put(Boolean'Image(TFORMn(I).Read) & " TFORM" & Integer'Image(I)&" ");
-- TIO.Put_Line(TFORMn(I).Value);
	if(MandVals.TFORMn(I).Read) then Put(Positive'Image(I) &":"& MandVals.TFORMn(I).Value & " "); end if;
end loop;
New_Line;
TIO.Put("TBCOL: ");
for I in MandVals.TBCOLn'Range
loop
-- TIO.Put(Boolean'Image(TFORMn(I).Read) & " TFORM" & Integer'Image(I)&" ");
-- TIO.Put_Line(TFORMn(I).Value);
	if(MandVals.TBCOLn(I).Read) then Put(Positive'Image(I) &":"& MandVals.TBCOLn(I).Value & " "); end if;
end loop;
New_Line;
--TIO.Put(Boolean'Image(MandVals.ENDCardSet) & " END ");
--TIO.Put_Line(Positive'Image(MandVals.ENDCardPos));
TIO.Put_Line(State_Name'Image(State.Name));
end DBG_Print;


-- type HDU_Type is
  --      (PRIMARY_WITHOUT_DATA, PRIMARY_IMAGE, RANDOM_GROUPS,
  --      EXT_IMAGE, EXT_ASCII_TABLE, EXT_BIN_TABLE, RANDOM_BLOCKS);
        function To_HDU_Type(XTENSION_Value : in String) return HDU_Type
        is
		t : HDU_Type;
        begin
                if(XTENSION_Value    = "'IMAGE   '          ") then
                                t := EXT_IMAGE;
				
                elsif(XTENSION_Value = "'TABLE   '          ") then
                                t := EXT_ASCII_TABLE;

                elsif(XTENSION_Value = "'BINTABLE'          ") then
                                t := EXT_BIN_TABLE;
		end if;
		-- FIXME RAND_BLOCKS ?
		
		return t;

        end To_HDU_Type;



	function Extract_Index(Root : String; CardKey : String) return Positive
	is
		RootLen : Positive := Root'Length;
	begin
		return Positive'Value( CardKey(RootLen+1 .. 8) );
	end Extract_Index;
-- utils end
-- -----------------------------------------------------------




	procedure Reset_State 
	is
	begin
		TIO.New_Line;
		State    := InitState;
		MandVals := InitMandVals; 	
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
			MandVals.XTENSION.Value := String(Card(11..30));
			MandVals.XTENSION.Read  := True;
			State.Name := CONFORMING_EXTENSION;
			State.XTENSION := To_HDU_Type(MandVals.XTENSION.Value);
		else
			State.Name := RANDOM_BLOCKS;
		end if;

		return Pos + 1;

	end In_INITIALIZED;






	function In_CONFORMING_EXTENSION(Pos : Positive; Card : Card_Type) return Positive
	is
	begin
		if    ( "BITPIX  " = String(Card(1..8)) AND (Pos = 2) )
		then
			MandVals.BITPIX.Value := String(Card(11..30));
			MandVals.BITPIX.Read  := True;

		elsif ( "NAXIS   " = String(Card(1..8)) AND (Pos = 3) )
		then
			MandVals.NAXIS.Value := String(Card(11..30));
			MandVals.NAXIS.Read  := True;

			State.NAXIS_Val      := Natural'Value(MandVals.NAXIS.Value);
			State.Name := COLLECT_NAXIS_ARRAY;
	
		elsif ( "PCOUNT  " = String(Card(1..8)) AND (Pos = 3 + State.NAXIS_Val + 1))
		then
			MandVals.PCOUNT.Value := String(Card(11..30));
			MandVals.PCOUNT.Read  := True;

		elsif ( "GCOUNT  " = String(Card(1..8)) AND (Pos = 3 + State.NAXIS_Val + 2))
		then
			MandVals.GCOUNT.Value := String(Card(11..30));
			MandVals.GCOUNT.Read  := True;

			case(State.XTENSION) is
				when EXT_ASCII_TABLE | EXT_BIN_TABLE =>
					State.Name := COLLECT_TABLE_ARRAYS;

				when EXT_IMAGE =>
					State.Name := WAIT_END;
					
				when others =>
					-- ERROR unknwon conforming extension
					null;
			end case;

		else
			-- ERROR unexpected card
			null;
		end if;

		return Pos + 1;

	end In_CONFORMING_EXTENSION;
	






	function In_COLLECT_NAXIS_ARRAY(Pos : Positive; Card : Card_Type) return Positive
	is
		Ix : Positive := Extract_Index("NAXIS",String(Card(1..8)));
	begin
		-- check root-name and position
		if ( ("NAXIS" = Card(1..5)) AND 
		     (Pos = 3 + Ix) )
		then
			MandVals.NAXISn(Ix).Value := String(Card(11..30));
			MandVals.NAXISn(Ix).Read  := True;
		else
			-- ERROR unexpected card
			null;
		end if;

		-- if last array card read, change state
		-- and read PCOUNT GCOUNT
		if(Ix = State.NAXIS_Val) 
		then
			State.Name := CONFORMING_EXTENSION;
		end if;

		return Pos + 1;

	end In_COLLECT_NAXIS_ARRAY;
	
	
	
	
	
		
	function In_COLLECT_TABLE_ARRAYS(Pos : Positive; Card : Card_Type) return Positive
	is
		Ix : Positive := 1;
	begin
		if ("TFIELDS " = String(Card(1..8)) )
                then
	               	MandVals.TFIELDS.Value := String(Card(11..30));
                        MandVals.TFIELDS.Read  := True;
			State.TFIELDS_Val := Natural'Value(MandVals.TFIELDS.Value);
                else
			-- ERROR unexpected card
			null;
		end if;

		-- check root-name
		if ( ("TFORM" = Card(1..5)) )
		then
			Ix := Extract_Index("TFORM",String(Card(1..8)));
			MandVals.TFORMn(Ix).Value := String(Card(11..30));
			MandVals.TFORMn(Ix).Read := True;
		else
			-- ERROR unexpected card
			null;
		end if;

		if(State.XTENSION = EXT_ASCII_TABLE) 
		then
			if ( ("TBCOL" = Card(1..5)) )
			then
				Ix := Extract_Index("TBCOL",String(Card(1..8)));
				MandVals.TBCOLn(Ix).Value := String(Card(11..30));
				MandVals.TBCOLn(Ix).Read  := True;
			else
				-- ERROR unexpected card
				null;
			end if;
		end if;

		-- if last array card read, change state
		if(Ix = State.TFIELDS_Val) 
		then
			State.Name := WAIT_END;
		end if;

		return Pos + 1;

	end In_COLLECT_TABLE_ARRAYS;
	


	function In_WAIT_END(Pos : Positive; Card : Card_Type) return Natural
	is
	begin
		if( ENDCard = Card ) then
			MandVals.ENDCardPos := Pos;
			MandVals.ENDCardSet := True;
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
		InState : State_Name := State.Name;
	begin

		case(State.Name) is
			when INITIALIZED => 
				NextCardPos := In_INITIALIZED(Pos, Card);

			when RANDOM_BLOCKS =>
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
		
		if(NextCardPos=0)then DBG_Print; end if;

		-- ask for next card from this position
		return NextCardPos;

	end Next;




   
	-- Get interface
	

        function  Get return HDU_Size_Info_Type
        is
                HDUSizeInfo : HDU_Size_Info_Type;
                NAXIS : Positive;
        begin
                HDUSizeInfo.HDUType    := State.XTENSION;
                -- will raise exception if state not PRIMARY* or RAND Groups

                if(MandVals.ENDCardSet) then
                        HDUSizeInfo.CardsCount := MandVals.ENDCardPos;
                else
                        null;
                        -- ERROR raise exception No END card found
                end if;

                if(MandVals.BITPIX.Read) then
                        HDUSizeInfo.BITPIX := Integer'Value(MandVals.BITPIX.Value);
                else
                        null;
                        -- ERROR raise exception No BITPIX card found
                end if;

                if(MandVals.NAXIS.Read) then
                        NAXIS := Integer'Value(MandVals.NAXIS.Value);
                else
                        null;
                        -- ERROR raise exception No NAXIS card found
                end if;

                for I in 1 .. NAXIS
                loop
                        if(MandVals.NAXISn(I).Read) then
                                HDUSizeInfo.NAXISArr(I) := Positive'Value(MandVals.NAXISn(I).Value);
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



