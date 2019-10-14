with Ada.Text_IO; -- for debug only DBG_Print, Trace_State

with Ada.Exceptions; use Ada.Exceptions;

with Keyword_Record; use Keyword_Record;



package body FA_Primary is

EmptyVal : constant String(1..20) := (others => ' ');
InitVal  : constant CardValue := (EmptyVal,False);

InitNAXISArrVal : constant NAXIS_Arr := (others => InitVal);

InitState : State_Type := 
	(NOT_ACCEPTING_CARDS, 0, 0,

        InitVal,InitVal,InitVal,InitVal,
        InitNAXISArrVal,
        InitVal,InitVal,InitVal,
        0,False);

State : State_Type := InitState;
------------------------------------------------------------------
package TIO renames Ada.Text_IO;

procedure DBG_Print 
is
begin
TIO.New_Line;
TIO.Put(Boolean'Image(State.SIMPLE.Read) & " SIMPLE ");
TIO.Put_Line(State.SIMPLE.Value);
TIO.Put(Boolean'Image(State.BITPIX.Read) & " BITPIX ");
TIO.Put_Line(State.BITPIX.Value);
TIO.Put(Boolean'Image(State.NAXIS.Read) & " NAXIS ");
TIO.Put_Line(State.NAXIS.Value);
TIO.Put(Boolean'Image(State.NAXIS1.Read) & " NAXIS1 ");
TIO.Put_Line(State.NAXIS1.Value);
for I in State.NAXISn'Range
loop
	if(State.NAXISn(I).Read) then
		TIO.Put(Integer'Image(I) &":"& State.NAXISn(I).Value);
	end if;
end loop;
TIO.New_Line;
TIO.Put(Boolean'Image(State.PCOUNT.Read) & " PCOUNT ");
TIO.Put_Line(State.PCOUNT.Value);
TIO.Put(Boolean'Image(State.GCOUNT.Read) & " GCOUNT ");
TIO.Put_Line(State.GCOUNT.Value);
TIO.Put(Boolean'Image(State.GROUPS.Read) & " GROUPS ");
TIO.Put_Line(State.GROUPS.Value);
TIO.Put(Boolean'Image(State.ENDCardSet) & " END ");
TIO.Put_Line(Positive'Image(State.ENDCardPos));
TIO.Put_Line(State_Name'Image(State.Name));
end DBG_Print;
-- -----------------------------------------------------------




	function Reset_State return Positive
	is
	begin
		State    := InitState;
		State.Name := PRIMARY_STANDARD;
		return 1; -- start FA from 1st card of HDU
	end Reset_State;





	function In_PRIMARY_STANDARD
		(Pos  : in Positive;
		 Card : in Card_Type) return Natural
	is
		Idx : Positive;
	begin
		TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));

		if ((Card(1..8) = "SIMPLE  ") AND (Pos = 1))
		then 
        		State.SIMPLE.Value := Card(11..30);
			State.SIMPLE.Read  := True;
			
			-- SIMPLE = F 
			-- non-standard primary HDU: don't know what to do -> exit.	
			if(To_Boolean(Card(11..30)) = False)
			then
				Raise_Exception(Unexpected_Card_Value'Identity, Card);
			end if;


		elsif ((Card(1..8) = "BITPIX  ") AND (Pos = 2))
		then
			State.BITPIX.Value := String(Card(11..30));
			State.BITPIX.Read  := True;
		
		elsif ((Card(1..8) = "NAXIS   ") AND (Pos = 3))
		then

			State.NAXIS_Val := To_Integer(Card(11..30));

			State.NAXIS.Value := Card(11..30);
			State.NAXIS.Read := True;
	
			if (State.NAXIS_Val = 0) then
				State.Name := WAIT_END;
				-- FIXME check this behaviour against Standard
				-- there is some talk that NAXISn() may also be zero
			end if;

		elsif ((Card(1..8) = "NAXIS1  ") AND (Pos = 4))
		then

			State.NAXIS1_Val := To_Integer(Card(11..30));

			State.NAXISn(1).Value := Card(11..30);
			State.NAXISn(1).Read := True;
	
		elsif (Is_Array(Card,"NAXIS",2,NAXIS_Max,Idx))
		then
			if(Pos = 3 + Idx)
			then
				State.NAXISn(Idx).Value := Card(11..30);
				State.NAXISn(Idx).Read  := True;
			else
				Raise_Exception(Unexpected_Card'Identity, Card);
			end if;
			
			if(Idx = State.NAXIS_Val)
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








        function In_WAIT_END(Pos : Positive; Card : Card_Type) return Natural
        is
        begin
                if( ENDCard = Card ) then
                        State.ENDCardPos := Pos;
                        State.ENDCardSet := True;
  		
			TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));

			if(State.NAXIS_Val = 0)
			then
				State.Name := NO_DATA;
			else
				State.Name := IMAGE;
			end if;

                        return 0; -- no more cards
                else
                        return Pos + 1;
                end if;
        end In_WAIT_END;








	function In_DATA_NOT_IMAGE
		(Pos  : in Positive;
		 Card : in Card_Type) return Natural
	is
		TripleComplete : Boolean := False;
	begin
		if (Card(1..8) = "GROUPS  ") then

			State.GROUPS.Value := String(Card(11..30));
			State.GROUPS.Read := True;

			TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));

			-- GROUPS = F
			if(To_Boolean(Card(11..30)) = False)
			then
				Raise_Exception(Unexpected_Card_Value'Identity, Card);
			end if;
			
		elsif (Card(1..8) = "PCOUNT  ") then
			State.PCOUNT.Value := Card(11..30);
			State.PCOUNT.Read  := True;
			
			TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));

		elsif (Card(1..8) = "GCOUNT  ") then
			State.GCOUNT.Value := String(Card(11..30));
			State.GCOUNT.Read  := True;
			
			TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));
	
		elsif (Card = ENDCard) then
			State.ENDCardPos := Pos;
                        State.ENDCardSet := True;
			
		        -- FIXME check on completeness move to Interpret.adb
			-- here simply set END card found
			TripleComplete :=
                                State.GROUPS.Read AND State.PCOUNT.Read AND State.GCOUNT.Read;

               		if (NOT TripleComplete)
                        then
                                Raise_Exception(Unexpected_Card'Identity, Card);
                        else
				TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8)
				                              &" "&Boolean'Image(TripleComplete) );
				State.Name := RANDOM_GROUPS;
			end if;



		end if;

		return Pos + 1;

	end In_DATA_NOT_IMAGE;









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
	
	

       function  Get return State_Type
       is
       begin
	       return State;
       end Get;

end FA_Primary;


