with Ada.Text_IO; -- for debug only DBG_Print, Trace_State

with Ada.Exceptions; use Ada.Exceptions;

with Keyword_Record; use Keyword_Record;

with Reserved; 


-- FIXME consider configurable how to react to duplicates (with the same card value)
-- FIXME Reserved (optional) keys are not fixed-format, but free-format -> Value NOT Card(11..30) !!
-- FIXME some keys may appear only in Primery (EXTEND,BLOCKED...?) and MUST NOT appear in extensions: should we check in extensions that they are not there ?



package body FA_Primary is


m_Options : Options_Type := (others => False);


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


type NAXIS_Arr is array (1..NAXIS_Max) of CardValue;

InitNAXISArrVal : constant NAXIS_Arr := (others => InitVal);
InitRANDGArrVal : constant RANDG_Arr := (others => InitVal);


type State_Type is
        record
	PrevPos : Natural;

        Name       : State_Name;
        NAXIS_Val  : Natural;
        NAXIS1_Val : Natural;

	-- Mandatory
        SIMPLE : CardValue;
        BITPIX : CardValue;
        NAXIS  : CardValue;
        NAXISn : NAXIS_Arr;
        PCOUNT : CardValue;
        GCOUNT : CardValue;
        GROUPS : CardValue;

	-- Reserved
	Res : Reserved.Primary_Type;

	-- other cards not recognized by this FA	
	OtherCount : Natural;

        ENDCardPos : Natural;
        ENDCardSet : Boolean;
        end record;

InitState : State_Type := 
	(
	0,
	NOT_ACCEPTING_CARDS, 0, 0,

        InitVal,InitVal,InitVal,
        InitNAXISArrVal,
        InitVal,InitVal,InitVal,
	Reserved.PrimInit,
	0,
        0,False);

State : State_Type := InitState;
------------------------------------------------------------------
package TIO renames Ada.Text_IO;

procedure DBG_Print 
is
begin
TIO.New_Line;
if(State.SIMPLE.Read) then TIO.Put_Line("SIMPLE  "&State.SIMPLE.Value); end if;
if(State.BITPIX.Read) then TIO.Put_Line("BITPITX "&State.BITPIX.Value); end if;
if(State.NAXIS.Read)  then TIO.Put_Line("NAXIS   "&State.NAXIS.Value);  end if;
for I in State.NAXISn'Range
loop
	if(State.NAXISn(I).Read) then
		TIO.Put(Integer'Image(I) &":"& State.NAXISn(I).Value);
	end if;
end loop;
TIO.New_Line;
if(State.PCOUNT.Read) then TIO.Put_Line("PCOUNT  "&State.PCOUNT.Value); end if;
if(State.GCOUNT.Read) then TIO.Put_Line("GCOUNT  "&State.GCOUNT.Value); end if;
if(State.GROUPS.Read) then TIO.Put_Line("GROUPS  "&State.GROUPS.Value); end if;
Reserved.DBG_Print(State.Res);
TIO.Put(Boolean'Image(State.ENDCardSet) & " END ");
TIO.Put_Line(Positive'Image(State.ENDCardPos));
TIO.Put_Line(State_Name'Image(State.Name));
end DBG_Print;


procedure DBG_Print_Reserved
is
	Res : Key_Rec_Arr := Get((DATAMAX,DATAMIN,INSTRUME,TELESCOP));
begin
	for I in Res'Range
	loop
		TIO.Put("Get Res> "&Reserved_Key'Image(Res(I).Key));
		TIO.Put(" : "&Res(I).Value);
		TIO.New_Line;
	end loop;


end DBG_Print_Reserved;

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

		TIO.Put_Line("Opts: "
		&" "&Boolean'Image(m_Options.Mand)
		&" "&Boolean'Image(m_Options.Reserved)
		);

		if(m_Options.Mand) 
		then
			State.Name := PRIMARY_STANDARD;

		elsif(m_Options.Reserved)
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
		-- Reserved (generic)

                if( Reserved.Match_Any(m_Options.Reserved,Pos,Card,State.Res))
		then
                      TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));

		-- Reserved (generic, image-like only)

                elsif( Reserved.Match_Any_DataArr(m_Options.Reserved,Card,State.Res.Res))
		then
                      TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));


		elsif( ENDCard = Card ) then
                        	State.ENDCardPos := Pos;
                        	State.ENDCardSet := True;
  		
			TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));

			if( State.NAXIS_Val = 0 )
			then
				State.Name := NO_DATA;
			else  
				State.Name := IMAGE;
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

		-- Mandatory keys

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


		-- Reserved keys (specific to RANDOM GROUPS)

		elsif(Reserved.Match_Any_ResRG(m_Options.Reserved, Card, State.Res.Arr))
		then
                      TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));
 

		-- Reserved keys (generic)

                elsif( Reserved.Match_Any(m_Options.Reserved,Pos,Card,State.Res))
		then
                      TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));

		-- Reserved (generic, data-arryas only)

                elsif( Reserved.Match_Any_DataArr(m_Options.Reserved,Card,State.Res.Res))
		then
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
		
		if(NextCardPos = 0) then DBG_Print; end if;
		if(NextCardPos = 0) then DBG_Print_Reserved; end if;

		return NextCardPos;

	end Next;





	
-- read Mandatory keys as record

	function To_Primary_HDU(S : State_Name) return Primary_HDU
	is
	begin
		case(S) is
			when NO_DATA => return NO_DATA;
			when IMAGE   => return IMAGE;
			when RANDOM_GROUPS => return RANDOM_GROUPS;
			when others =>
				Raise_Exception(Programming_Error'Identity, "Get called but header not read or only partially read.");
		end case;
	end To_Primary_HDU;


	function  Get return Primary_Size_Rec
	is
		PrimSize : Primary_Size_Rec(State.NAXIS_Val,
						To_Primary_HDU(State.Name));
                NAXIS : Positive;
        begin
                if(State.ENDCardSet) then
                        PrimSize.CardsCount := State.ENDCardPos;
                else
                        Raise_Exception(Card_Not_Found'Identity, 
					"END card not found, not a valid FITS file.");
                end if;


                PrimSize.BITPIX := To_Integer(State.BITPIX.Value);

                NAXIS := To_Integer(State.NAXIS.Value);
               	for I in 1 .. NAXIS
               	loop
                       	if(State.NAXISn(I).Read) then
                         		PrimSize.NAXISArr(I) := To_Integer(State.NAXISn(I).Value);
                       	else
                               	Raise_Exception(Card_Not_Found'Identity, "NAXIS"&Integer'Image(I));
                       	end if;

               	end loop;

		case(PrimSize.HDUType) is
			when RANDOM_GROUPS =>
				PrimSize.PCOUNT := To_Integer(State.PCOUNT.Value);
				PrimSize.GCOUNT := To_Integer(State.GCOUNT.Value);
			when others =>
				null;
		end case;

 		return PrimSize;
        end Get;











-- read reserved scalar keys

 function Needed_Count(Keys : in Res_Key_Arr) return Natural
 is
	 Count : Natural := 0;
 begin
	for I in Keys'Range
	loop
		if(State.Res.Res(Keys(I)).Read)
		then
			Count := Count + 1;
		end if;
	end loop;

	return Count;

 end Needed_Count;


 function Get(Keys : in Res_Key_Arr) return Key_Rec_Arr
 is
	 FoundCount : Natural := Needed_Count(Keys);
	 OutKeys : Key_Rec_Arr(1..FoundCount);
	 Idx : Positive := 1; 
 begin

	for I in Keys'Range
	loop
		if(State.Res.Res(Keys(I)).Read)
		then
			OutKeys(Idx).Key   := Keys(I);
			OutKeys(Idx).Value := State.Res.Res(Keys(I)).Value;
			exit when (Idx = FoundCount);
			Idx := Idx + 1;
		end if;
	end loop;

	return OutKeys;
 end Get;


        -- experimental Get(RootArr) to return Tab_Type's read arrays

        -- FIXME modify so that Arr can be shorter, containing only the Read elements
        function Is_Any_Element_Read(Arr : Reserved.RANDG_Arr) return Boolean
        is
                Is_Read : Boolean := False;
                -- FIXME what if Arr is empty array ? raise exception or return False
        begin
                for I in Arr'Range
                loop
                        Is_Read := Arr(I).Read;
                        exit when Arr(I).Read;
                end loop;
                return Is_Read;
        end Is_Any_Element_Read;


       function Is_In_Set(Root : Root_Type; Roots : Root_Arr) return Boolean
        is
        begin
                for I in Roots'Range
                loop
                        if(Root = Roots(I))
                        then
                                return True;
                        end if;
                end loop;
                return False;
        end Is_In_Set;

        function Needed_Length(Roots : Root_Arr) return Natural
        is
                Len : Natural := 0;
        begin
                if(Is_In_Set(PTYPE,Roots)) then
                        if(Is_Any_Element_Read(State.Res.Arr(Reserved.PTYPE))) then Len := Len + 1; end if;
                end if;
                if(Is_In_Set(PSCAL,Roots)) then
                        if(Is_Any_Element_Read(State.Res.Arr(Reserved.PSCAL))) then Len := Len + 1; end if;
                end if;
                if(Is_In_Set(PZERO,Roots)) then
                        if(Is_Any_Element_Read(State.Res.Arr(Reserved.PZERO))) then Len := Len + 1; end if;
                end if;
                return Len;
        end Needed_Length;






 function Get(Roots : Root_Arr) return IdxKey_Rec_Arr
        is
                IdxKey : IdxKey_Rec_Arr(1..Needed_Length(Roots));
                Len : Natural := 0;-- FIXME rename to Idx
        begin
                if(Is_In_Set(PTYPE,Roots))
                then
                        if(Is_Any_Element_Read(State.Res.Arr(Reserved.PTYPE)))
                        then
                                Len := Len + 1;
                                IdxKey(Len).Root := PTYPE;
                                IdxKey(Len).Arr  := State.Res.Arr(Reserved.PTYPE);
                        end if;
                end if;

                if(Is_In_Set(PSCAL,Roots))
                then
                        if(Is_Any_Element_Read(State.Res.Arr(Reserved.PSCAL)))
                        then
                                Len := Len + 1;
                                IdxKey(Len).Root := PSCAL;
                                IdxKey(Len).Arr  := State.Res.Arr(Reserved.PSCAL);
                        end if;
                end if;

                if(Is_In_Set(PZERO,Roots))
                then
                        if(Is_Any_Element_Read(State.Res.Arr(Reserved.PZERO)))
                        then 
                                Len := Len + 1;
                                IdxKey(Len).Root := PZERO;
                                IdxKey(Len).Arr  := State.Res.Arr(Reserved.PZERO);
                        end if;
                end if;

        return IdxKey;
        end Get;


end FA_Primary;


