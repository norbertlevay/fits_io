
-- Mandiatory keywords have values in Fixed format. No complex parsing is needed.
-- Simple string comparison is enough to make decisions.


with Ada.Text_IO; -- for debug only DBG_Print
with Ada.Exceptions; use Ada.Exceptions;

with Keyword_Record; use Keyword_Record;

package body FA_Primary is

EmptyVal : constant String(1..20) := (others => ' ');
InitVal  : constant CardValue := (EmptyVal,False);

InitNAXISArrVal : constant NAXIS_Arr := (others => InitVal);

InitMandVals : Primary_Mandatory_Card_Values := (UNSPECIFIED,False,
						 InitVal,InitVal,InitVal,InitVal,
                                                 InitNAXISArrVal,
						 InitVal,InitVal,InitVal,
						 0,False);

MandVals : Primary_Mandatory_Card_Values;

type State_Name is (
	NOT_ACCEPTING_CARDS, -- FA inactive
	PRIMARY_STANDARD, -- expect PrimStandard; check cardkey and position
	RANDOM_GROUPS,    -- expect RandGroups;   check cardkey only
	WAIT_END	  -- check END-card only
	);

type State_Type is
	record
		Name       : State_Name;
		NAXIS_Val  : Natural;
		NAXIS1_Val : Natural;
	end record;
-- collects values used in state-change 
-- decisions in different states

InitState : State_Type := (Name => NOT_ACCEPTING_CARDS, NAXIS_Val => 0, NAXIS1_Val => 0);

State : State_Type := InitState;
------------------------------------------------------------------
package TIO renames Ada.Text_IO;

procedure DBG_Print 
is
begin
TIO.New_Line;
TIO.Put(Boolean'Image(MandVals.SIMPLE.Read) & " SIMPLE ");
TIO.Put_Line(MandVals.SIMPLE.Value);
TIO.Put(Boolean'Image(MandVals.BITPIX.Read) & " BITPIX ");
TIO.Put_Line(MandVals.BITPIX.Value);
TIO.Put(Boolean'Image(MandVals.NAXIS.Read) & " NAXIS ");
TIO.Put_Line(MandVals.NAXIS.Value);
TIO.Put(Boolean'Image(MandVals.NAXIS1.Read) & " NAXIS1 ");
TIO.Put_Line(MandVals.NAXIS1.Value);
for I in MandVals.NAXISn'Range
loop
-- TIO.Put(Boolean'Image(MandVals.NAXISn(I).Read) & " NAXIS" & Integer'Image(I)&" ");
-- TIO.Put_Line(MandVals.NAXISn(I).Value);
	if(MandVals.NAXISn(I).Read) then
		TIO.Put(Integer'Image(I) &":"& MandVals.NAXISn(I).Value);
	end if;
end loop;
TIO.New_Line;
TIO.Put(Boolean'Image(MandVals.PCOUNT.Read) & " PCOUNT ");
TIO.Put_Line(MandVals.PCOUNT.Value);
TIO.Put(Boolean'Image(MandVals.GCOUNT.Read) & " GCOUNT ");
TIO.Put_Line(MandVals.GCOUNT.Value);
TIO.Put(Boolean'Image(MandVals.GROUPS.Read) & " GROUPS ");
TIO.Put_Line(MandVals.GROUPS.Value);
TIO.Put(Boolean'Image(MandVals.ENDCardSet) & " END ");
TIO.Put_Line(Positive'Image(MandVals.ENDCardPos));
TIO.Put_Line(State_Name'Image(State.Name));
end DBG_Print;
-- -----------------------------------------------------------








	function Reset_State return Positive
	is
	begin
		MandVals := InitMandVals;
		State    := InitState;
		State.Name := PRIMARY_STANDARD;
		return 1; -- start FA from 1st card of HDU
	end Reset_State;




	--
	-- State Transition functions
	--

	function In_PRIMARY_STANDARD
		(Pos  : in Positive;
		 Card : in Card_Type) return Natural
	is
		Idx : Positive;
	begin
		if ((Card(1..8) = "SIMPLE  ") AND (Pos = 1))
		then 
        		MandVals.SIMPLE.Value := Card(11..30);
			MandVals.SIMPLE.Read  := True;
	
			declare
				CardVal : Boolean := To_Boolean(Card(11..30));
			begin
				if(CardVal = False) 
				then 
					State.Name := NOT_ACCEPTING_CARDS;
					MandVals.HDUTypeVal := PRIMARY_NON_STANDARD;
					MandVals.HDUTypeSet := True;
				end if;
			end;


		elsif ((Card(1..8) = "BITPIX  ") AND (Pos = 2))
		then
			MandVals.BITPIX.Value := String(Card(11..30));
			MandVals.BITPIX.Read  := True;
		
		elsif ((Card(1..8) = "NAXIS   ") AND (Pos = 3))
		then

			State.NAXIS_Val := To_Integer(Card(11..30));

			MandVals.NAXIS.Value := Card(11..30);
			MandVals.NAXIS.Read := True;
	
			if (State.NAXIS_Val = 0) then
				State.Name := WAIT_END;
				MandVals.HDUTypeVal := PRIMARY_WITHOUT_DATA;
				MandVals.HDUTypeSet := True;
				-- FIXME check this behaviour against Standard
				-- there is some talk that NAXISn() may also be zero
			end if;

		elsif ((Card(1..8) = "NAXIS1  ") AND (Pos = 4))
		then

			State.NAXIS1_Val := To_Integer(Card(11..30));

			MandVals.NAXISn(1).Value := Card(11..30);
			MandVals.NAXISn(1).Read := True;
	
		elsif (Is_Array(Card,"NAXIS",2,NAXIS_Max,Idx))
		then
			if(Pos = 3 + Idx)
			then
				MandVals.NAXISn(Idx).Value := Card(11..30);
				MandVals.NAXISn(Idx).Read  := True;
			else
				Raise_Exception(Unexpected_Card'Identity, Card);
			end if;
			
			if(Idx = State.NAXIS_Val)
			then
				if (State.NAXIS1_Val = 0) then
					State.Name := RANDOM_GROUPS;
					MandVals.HDUTypeVal := PRIMARY_NOT_IMAGE;
					MandVals.HDUTypeSet := True;
				else
					State.Name := WAIT_END;
					MandVals.HDUTypeVal := PRIMARY_IMAGE;
					MandVals.HDUTypeSet := True;
				end if;
			end if;

		else
			Raise_Exception(Unexpected_Card'Identity, Card);
		end if;
		
		return Pos + 1;

	end In_PRIMARY_STANDARD;








	function In_RANDOM_GROUPS
		(Pos  : in Positive;
		 Card : in Card_Type) return Natural
	is
	begin
		if (Card(1..8) = "GROUPS  ") then

			MandVals.GROUPS.Value := String(Card(11..30));
			MandVals.GROUPS.Read := True;

			declare
				CardVal : Boolean := To_Boolean(Card(11..30));
			begin
				if(CardVal = True) then
					MandVals.HDUTypeVal := RANDOM_GROUPS;
					MandVals.HDUTypeSet := True;
				else
					Raise_Exception(Unexpected_Card_Value'Identity, Card);
				end if;
			end;
			
		elsif (Card(1..8) = "PCOUNT  ") then
			MandVals.PCOUNT.Value := Card(11..30);
			MandVals.PCOUNT.Read  := True;

		elsif (Card(1..8) = "GCOUNT  ") then
			MandVals.GCOUNT.Value := String(Card(11..30));
			MandVals.GCOUNT.Read  := True;
	

		end if;


		declare	
			TripleComplete : Boolean := 
				MandVals.GROUPS.Read AND MandVals.PCOUNT.Read AND MandVals.GCOUNT.Read;
		begin
			if(Card = ENDCard AND NOT TripleComplete) then
				Raise_Exception(Unexpected_Card'Identity, Card);
			end if;

			if(TripleComplete)
			then
				State.Name := WAIT_END;
			end if;
		end;

		return Pos + 1;

	end In_RANDOM_GROUPS;




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
	function  Next
		(Pos  : in Positive; 
		 Card : in Card_Type) return Natural
	is
		NextCardPos : Natural;
	begin

		case(State.Name) is
			when PRIMARY_STANDARD => 
			       NextCardPos := In_PRIMARY_STANDARD(Pos, Card);
			when RANDOM_GROUPS => 
			       NextCardPos := In_RANDOM_GROUPS(Pos, Card);
			when WAIT_END =>
			       NextCardPos := In_WAIT_END(Pos, Card);
			when NOT_ACCEPTING_CARDS =>
			       NextCardPos := 0;
		end case;

		 if(NextCardPos = 0) then DBG_Print; end if;
		
		return NextCardPos;

	end Next;
	

	
	--
	-- Collect Results
	--

       function  Get return Primary_Mandatory_Card_Values
       is
       begin
	       return MandVals;
       end Get;

end FA_Primary;


