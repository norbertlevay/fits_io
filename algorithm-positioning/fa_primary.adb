with Ada.Text_IO; -- for debug only DBG_Print, Trace_State

with Ada.Exceptions; use Ada.Exceptions;

with Keyword_Record; use Keyword_Record;

with Reserved; 


package body FA_Primary is


m_Options : Options_Type := (False, False, False);


EmptyVal : constant String(1..20) := (others => ' ');

        --
        -- definition of states
        --

type State_Name is 
       (NOT_ACCEPTING_CARDS, -- FA inactive
        PRIMARY_STANDARD,    -- Initial state: collect scalar card-values
        DATA_NOT_IMAGE,      -- collect GROUPS PCOUNT GCOUNT and END-card
        WAIT_END,            -- ignore all cards except END-card
        NO_DATA, IMAGE, RANDOM_GROUPS -- Final states
        );


type CardValue is
        record
                Value : String(1..20);
                Read  : Boolean;
        end record;

InitVal  : constant CardValue := (EmptyVal,False);

type NAXIS_Arr is array (1..NAXIS_Max) of CardValue;

InitNAXISArrVal : constant NAXIS_Arr := (others => InitVal);


type State_Type is
        record
	PrevPos : Natural;

        Name       : State_Name;
        NAXIS_Val  : Natural;
        NAXIS1_Val : Natural;

        SIMPLE : CardValue;
        BITPIX : CardValue;
        NAXIS  : CardValue;
        NAXIS1 : CardValue;
        NAXISn : NAXIS_Arr;
        PCOUNT : CardValue;
        GCOUNT : CardValue;
        GROUPS : CardValue;
	
	Obs : Reserved.Obs_Type;

	OtherCount : Natural;

        ENDCardPos : Natural;
        ENDCardSet : Boolean;
        end record;

InitState : State_Type := 
	(
	0,
	NOT_ACCEPTING_CARDS, 0, 0,

        InitVal,InitVal,InitVal,InitVal,
        InitNAXISArrVal,
        InitVal,InitVal,InitVal,
	Reserved.InitObs,
	0,
        0,False);

State : State_Type := InitState;
------------------------------------------------------------------
package TIO renames Ada.Text_IO;

procedure DBG_Print 
is
begin
TIO.New_Line;
--TIO.Put_Line("Config:Options: " & Options_Type'Image(m_Options));
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
Reserved.DBG_Print(State.Obs);
TIO.Put(Boolean'Image(State.ENDCardSet) & " END ");
TIO.Put_Line(Positive'Image(State.ENDCardPos));
TIO.Put_Line(State_Name'Image(State.Name));
end DBG_Print;
-- -----------------------------------------------------------

        procedure Configure(Options : Options_Type)
	is
	begin
		m_Options := Options;
	end Configure;






--
-- state transitions
--



	function Reset_State return Positive
	is
	begin
		State := InitState;
		if(m_Options.Mand) 
		then
			State.Name := PRIMARY_STANDARD;

		elsif(m_Options.Biblio)
		then
			Raise_Exception(Programming_Error'Identity, 
				"Option Biblie not implemented.");	

		elsif(m_Options.Obs)
		then
			State.Name := WAIT_END;

		else
			State.Name := WAIT_END;
		end if;
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
			
			if(Idx >= State.NAXIS_Val)
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
                if( m_Options.Obs AND Reserved.Match_Any_Obs(Card,State.Obs)) then
			null;

		elsif( ENDCard = Card ) then
                        	State.ENDCardPos := Pos;
                        	State.ENDCardSet := True;
  		
			TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));

			if( m_Options.Mand AND State.NAXIS_Val = 0 )
			then
				State.Name := NO_DATA;

			elsif( m_Options.Mand AND State.NAXIS_Val > 0 )
			then
				State.Name := IMAGE;
		
			else
				null;-- FIXME State.Name := NOT_DETERMINED;
			end if;

                        return 0;
			-- no more cards

		elsif(Is_Fixed_Position(Card)) then
			-- one of PRIMARY_STANDARD cards: may appear only once in header
			Raise_Exception(Duplicate_Card'Identity, Card);

		elsif(Is_Valid(Card)) then
			-- defined by [FITS Appendix A] BNF syntax
			State.OtherCount := State.OtherCount + 1;
			-- valid but unknown to this FA-implementation

		else
			Raise_Exception(Invalid_Card'Identity, Card);
			-- found card which does not confirm [FITS Addendix A] BNF syntax
			-- FIXME consider configurable whether to raise excpetion here or ignore
                end if;
	
		return Pos + 1;

        end In_WAIT_END;






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


	procedure Assert_GROUPS_T_Found(Card : in Card_Type)
	is
	begin
	-- GROUPS = F
		if(To_Boolean(Card(11..30)) = False)
		then
			Raise_Exception(Unexpected_Card_Value'Identity, Card);
		end if;
	end Assert_GROUPS_T_Found;


	function In_DATA_NOT_IMAGE
		(Pos  : in Positive;
		 Card : in Card_Type) return Natural
	is
	begin
		-- always check State.xxx.Read flag to avoid duplicates
		-- FIXME consider configurable how to react to duplicates (with the same card value)

		if ( Card(1..8) = "GROUPS  " ) then
			
			if (NOT State.GROUPS.Read)
			then
				State.GROUPS.Value := String(Card(11..30));
				State.GROUPS.Read := True;
			else
                                -- FIXME only duplicates with diff values raises exception
                                -- duplicate with equal values: make configurable what to do...
                                Raise_Exception(Duplicate_Card'Identity, Card);
			end if;

			TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));

			Assert_GROUPS_T_Found(Card);

		elsif (Card(1..8) = "PCOUNT  ") then
			
			if (NOT State.PCOUNT.Read)
			then
				State.PCOUNT.Value := Card(11..30);
				State.PCOUNT.Read  := True;
			else
                                Raise_Exception(Duplicate_Card'Identity, Card);
			end if;
			
			TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));

		elsif (Card(1..8) = "GCOUNT  ") then
			
			if (NOT State.GCOUNT.Read)
			then
				State.GCOUNT.Value := String(Card(11..30));
				State.GCOUNT.Read  := True;
			else
                                Raise_Exception(Duplicate_Card'Identity, Card);
			end if;
			
			TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));
	
		elsif (Card = ENDCard) then
			State.ENDCardPos := Pos;
                        State.ENDCardSet := True;
	
			TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));

			Assert_GROUPS_PCOUNT_GCOUNT_Found;
 
			State.Name := RANDOM_GROUPS;
                        return 0;
			-- no more cards


		elsif(Is_Fixed_Position(Card)) then
			
			Raise_Exception(Duplicate_Card'Identity, Card);
			-- one of PRIMARY_STANDARD cards: may appear only once in header

		elsif(Is_Valid(Card)) then
			-- valid card defined by [FITS Appendix A] BNF syntax
			State.OtherCount := State.OtherCount + 1;
			-- valid but unknown to this FA-implementation
		else
			Raise_Exception(Invalid_Card'Identity, Card);
			-- found card which does not confirm [FITS Addendix A] BNF syntax
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

			when PRIMARY_STANDARD => 
				NextCardPos := In_PRIMARY_STANDARD(Pos, Card);
			when WAIT_END =>
				NextCardPos := In_WAIT_END(Pos, Card);
			when DATA_NOT_IMAGE => 
				NextCardPos := In_DATA_NOT_IMAGE(Pos, Card);

			when NO_DATA | IMAGE | RANDOM_GROUPS =>
				NextCardPos := 0;
		end case;
		
		-- TIO.Put_Line("DBG> "&Integer'Image(NextCardPos) & Card);
		if(NextCardPos = 0) then DBG_Print; end if;

		return NextCardPos;

	end Next;




	function To_HDU_Type(StateName : in FA_Primary.State_Name) return HDU_Type
	is
		t : HDU_Type;
	begin
		case(StateName) is
			when NO_DATA 	   => t := PRIMARY_WITHOUT_DATA;
			when IMAGE   	   => t := PRIMARY_IMAGE;
			when RANDOM_GROUPS => t := RANDOM_GROUPS;
			when others =>
				Raise_Exception(Programming_Error'Identity,
				"Not all cards read. State "&
				FA_Primary.State_Name'Image(StateName) );
		end case;

		return t;

	end To_HDU_Type;



	function  Get return HDU_Size_Rec
	is
		HDUSizeInfo : HDU_Size_Rec;
                NAXIS : Positive;
        begin
-- Final FA states naming: 
-- IMAGE : header contains exactly only mandatory cards for IMAGE, no other cards.
-- IMAGE with other(n) : header has mandatory IMAGE cards and n extra cards 
-- not spec'd by Options or unknown to this implementation.
-- Other cases refer to Reserved key groups, like biblio, related to bibligraphic keys:
-- IMAGE with biblo wcs : has only mandatory keys and at least one of biblio related 
-- reserved keys, and some WCS keys.
		TIO.Put(State_Name'Image(State.Name));
		if(State.OtherCount > 0)
		then
			TIO.Put_Line(" with Other("& Integer'Image(State.OtherCount) &")");
		end if;


		-- NOTE: user can simply call Get() without running the FA -> programming error
		-- OR Header was read, but is broken (without END card) so we read through all file
		--  reaching EOF <- but this should raise exception: trying to read behind file end??
		--  OR user called FA on non FITS file and so probably scans through until EOF
                if(State.ENDCardSet) then
                        HDUSizeInfo.CardsCount := State.ENDCardPos;
                else
                        Raise_Exception(Card_Not_Found'Identity, 
					"END card not found, not a valid FITS file.");
                end if;


		--
		-- conversion HDU_Size_Rec
		--

                -- FIXME how about SIMPLE value, should we check it was set ?
		
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



end FA_Primary;

