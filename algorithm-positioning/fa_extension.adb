with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with FITS; use FITS;
-- Card_Type needed

with Keyword_Record; use Keyword_Record;



package body FA_Extension is

EmptyVal : constant String(1..20) := (others => ' ');
InitVal  : constant CardValue := (EmptyVal,False);

--
-- collected values
--
InitNAXISArrVal : constant NAXIS_Arr := (others => InitVal);
InitTFORMArrVal : constant TFORM_Arr := (others => InitVal);
InitTBCOLArrVal : constant TBCOL_Arr := (others => InitVal);

InitMandVals : Extension_Mandatory_Card_Values := (False,
						InitVal,InitVal,InitVal,
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
	(NOT_ACCEPTING_CARDS,   -- FA inactive
	 INITIALIZED,   -- Reset_State was callled, FA ready to accept cards
	 CONFORMING_EXTENSION, -- collect scalar card values
	 COLLECT_NAXIS_ARRAY,  -- collect NAXIS array values
	 COLLECT_TABLE_ARRAYS, -- collect TFORM & TBCOL array values
	 WAIT_END);     -- read cards until END card encoutered

type XT_Type is
        (UNSPECIFIED, IMAGE, ASCII_TABLE, BIN_TABLE);

type State_Type is 
	record
		Name        : State_Name;
		XTENSION    : XT_Type;
		NAXIS_Val   : Natural;
		TFIELDS_Val : Natural;
	end record;
-- collects values used in state-change
-- decisions in different states

InitState : State_Type := 
	(Name => NOT_ACCEPTING_CARDS, NAXIS_Val => 0, TFIELDS_Val => 0, XTENSION => UNSPECIFIED);

State : State_Type := InitState;

------------------------------------------------------------------
-- utils
--
package TIO renames Ada.Text_IO;

procedure DBG_Print
is
begin
TIO.New_Line;
TIO.Put_Line("SPECRECORDS found: "& Boolean'Image(MandVals.SPECRECORDS));
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
-- utils end
-- -----------------------------------------------------------




	function Reset_State return Positive
	is
	begin
		MandVals := InitMandVals; 
		State    := InitState;
		State.Name := INITIALIZED;
		return 1; -- start FA from Header's 1st card	
	end Reset_State;




	function In_INITIALIZED(Pos : Positive; Card : Card_Type) return Positive
	is
	begin
		-- [FITS 3.5] The first 8 bytes of the special records 
		-- must not contain the string “XTENSION”.

		if(Pos /= 1) then
			Raise_Exception(Programming_Error'Identity, 
			"First card expected but card position is " & Integer'Image(Pos));
		end if;

		if( "XTENSION" = Card(1..8) )
		then
			MandVals.XTENSION.Value := Card(11..30);
			MandVals.XTENSION.Read  := True;
			State.Name := CONFORMING_EXTENSION;
			State.XTENSION := To_XT_Type(MandVals.XTENSION.Value);
		else
			State.Name := NOT_ACCEPTING_CARDS;
			MandVals.SPECRECORDS := True;
			-- FIXME check this behaviour with Standard
		end if;

		return Pos + 1;

	end In_INITIALIZED;






	function In_CONFORMING_EXTENSION(Pos : Positive; Card : Card_Type) return Positive
	is
	begin
		if    ( "BITPIX  " = Card(1..8) AND (Pos = 2) )
		then
			MandVals.BITPIX.Value := Card(11..30);
			MandVals.BITPIX.Read  := True;

		elsif ( "NAXIS   " = Card(1..8) AND (Pos = 3) )
		then
			MandVals.NAXIS.Value := Card(11..30);
			MandVals.NAXIS.Read  := True;

			State.NAXIS_Val      := To_Integer(MandVals.NAXIS.Value);
			State.Name := COLLECT_NAXIS_ARRAY;
	
		elsif ( "PCOUNT  " = Card(1..8) AND (Pos = 3 + State.NAXIS_Val + 1))
		then
			MandVals.PCOUNT.Value := Card(11..30);
			MandVals.PCOUNT.Read  := True;

		elsif ( "GCOUNT  " = Card(1..8) AND (Pos = 3 + State.NAXIS_Val + 2))
		then
			MandVals.GCOUNT.Value := Card(11..30);
			MandVals.GCOUNT.Read  := True;

			case(State.XTENSION) is
				when ASCII_TABLE | BIN_TABLE =>
					State.Name := COLLECT_TABLE_ARRAYS;

				when IMAGE =>
					State.Name := WAIT_END;
					
				when UNSPECIFIED =>
					Raise_Exception(Programming_Error'Identity, "This should not hapen.");
			end case;

		else
			Raise_Exception(Unexpected_Card'Identity, Card);
		end if;

		return Pos + 1;

	end In_CONFORMING_EXTENSION;
	






	function In_COLLECT_NAXIS_ARRAY(Pos : Positive; Card : Card_Type) return Positive
	is
		Ix : Positive := Extract_Index("NAXIS",Card(1..8));
	begin

		if ( ("NAXIS" = Card(1..5)) AND (Pos = 3 + Ix) )
		then
			MandVals.NAXISn(Ix).Value := Card(11..30);
			MandVals.NAXISn(Ix).Read  := True;
		else
			Raise_Exception(Unexpected_Card'Identity, Card);
		end if;

		if(Ix = State.NAXIS_Val) 
		then
			State.Name := CONFORMING_EXTENSION;
			-- return to read PCOUNT GCOUNT
		end if;

		return Pos + 1;

	end In_COLLECT_NAXIS_ARRAY;
	
	
	
	
	
		
	function In_COLLECT_TABLE_ARRAYS(Pos : Positive; Card : Card_Type) return Positive
	is
		Ix : Positive := 1;
	begin
		if ("TFIELDS " = Card(1..8)  )
                then
			if (Pos = 3 + State.NAXIS_Val + 3)
			then
				MandVals.TFIELDS.Value := Card(11..30);
				MandVals.TFIELDS.Read  := True;
				State.TFIELDS_Val := To_Integer(MandVals.TFIELDS.Value);
			else
				Raise_Exception(Unexpected_Card'Identity, Card);
			end if;
		end if;

		if ( ("TFORM" = Card(1..5)) )
		then
			Ix := Extract_Index("TFORM",Card(1..8));
			MandVals.TFORMn(Ix).Value := Card(11..30);
			MandVals.TFORMn(Ix).Read := True;
		end if;

		if(State.XTENSION = ASCII_TABLE) 
		then
			if ( ("TBCOL" = Card(1..5)) )
			then
				Ix := Extract_Index("TBCOL",Card(1..8));
				MandVals.TBCOLn(Ix).Value := Card(11..30);
				MandVals.TBCOLn(Ix).Read  := True;
			end if;
		end if;

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
			State.Name := NOT_ACCEPTING_CARDS;
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
			when CONFORMING_EXTENSION =>
				NextCardPos := In_CONFORMING_EXTENSION(Pos, Card);
			when COLLECT_NAXIS_ARRAY =>
				NextCardPos := In_COLLECT_NAXIS_ARRAY(Pos, Card);
			when COLLECT_TABLE_ARRAYS =>
				NextCardPos := In_COLLECT_TABLE_ARRAYS(Pos, Card);
			when WAIT_END =>
				NextCardPos := In_WAIT_END(Pos, Card);
			when NOT_ACCEPTING_CARDS =>
				NextCardPos := 0;
		end case;
		
		if(NextCardPos = 0) then DBG_Print; end if;

		-- ask for next card from this position
		return NextCardPos;

	end Next;




   
	-- Get interface
        function  Get return Extension_Mandatory_Card_Values
	is
	begin
		return MandVals; 
	end Get;

end FA_Extension;



