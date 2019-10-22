with Ada.Text_IO; -- for debug only DBG_Print, Trace_State

with Ada.Exceptions; use Ada.Exceptions;

with Keyword_Record; use Keyword_Record;

with Reserved; 


-- FIXME consider configurable how to react to duplicates (with the same card value)
-- FIXME Reserved (optional) keys are not fixed-format, but free-format -> Value NOT Card(11..30) !!

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


type CardValue is
        record
                Value : String(1..20);
                Read  : Boolean;
        end record;

InitVal  : constant CardValue := (EmptyVal,False);

type NAXIS_Arr is array (1..NAXIS_Max) of CardValue;
type RANDG_Arr is array (1..RANDG_Max) of CardValue;

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

	-- Reserved (Primary only)
	EXTEND : CardValue;-- FIXME should we check: extension HDU must not have this ?
	-- Reserved (RANDOM GROUPS specific)
	PTYPEn : RANDG_Arr;
	PSCALn : RANDG_Arr;
	PZEROn : RANDG_Arr;
	-- Reserved (generic, data-arrays only, like IMAGE)
	DataArr : Reserved.DataArr_Type;
	-- Reserved (generic)
	GenRes : Reserved.Reserved_Type;

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
	InitVal,
	InitRANDGArrVal,InitRANDGArrVal,InitRANDGArrVal,
	Reserved.InitDataArr,
	Reserved.Init,
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
--TIO.Put(Boolean'Image(State.NAXIS1.Read) & " NAXIS1 "); TIO.Put_Line(State.NAXIS1.Value);
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
if(State.EXTEND.Read) then TIO.Put_Line("EXTEND  "&State.EXTEND.Value); end if;
Reserved.DBG_Print(State.GenRes);
Reserved.DBG_Print(State.DataArr);
TIO.Put(Boolean'Image(State.ENDCardSet) & " END ");
TIO.Put_Line(Positive'Image(State.ENDCardPos));
TIO.Put_Line(State_Name'Image(State.Name));
end DBG_Print;


procedure DBG_Print_Reserved
is
	Prod    : Prod_Arr    := Get;
	Biblio  : Biblio_Arr  := Get;
	Obs     : Obs_Arr     := Get;
	DataArr : DataArr_Arr := Get;
begin
	for I in Prod'Range
	loop
		TIO.Put("Get Prod> "&Prod_Key'Image(Prod(I).Key));
		TIO.Put(" : "&Prod(I).Value);
		TIO.New_Line;
	end loop;

	for I in Biblio'Range
	loop
		TIO.Put("Get Biblio> "&Biblio_Key'Image(Biblio(I).Key));
		TIO.Put(" : "&Biblio(I).Value);
		TIO.New_Line;
	end loop;

	for I in Obs'Range
	loop
		TIO.Put("Get Obs> "&Obs_Key'Image(Obs(I).Key));
		TIO.Put(" : "&Obs(I).Value);
		TIO.New_Line;
	end loop;

	for I in DataArr'Range
	loop
		TIO.Put("Get DataArr> "&DataArr_Key'Image(DataArr(I).Key));
		TIO.Put(" : "&DataArr(I).Value);
		TIO.New_Line;
	end loop;

end DBG_Print_reserved;

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
		-- Reserved (Primary only)

                if (Card(1..8) = "EXTEND  ") then

                        if (NOT State.EXTEND.Read)
                        then
                                State.EXTEND.Value := String(Card(11..30));
                                State.EXTEND.Read  := True;
                        else
                                Raise_Exception(Duplicate_Card'Identity, Card);
                        end if;

                        TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));

		-- Reserved (generic)

                elsif( Reserved.Match_Any(m_Options.Reserved,Pos,Card,State.GenRes))
		then
                      TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));

		-- Reserved (generic, image-like only)

                elsif( Reserved.Match_Any_DataArr(m_Options.Reserved,Card,State.DataArr))
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
		Idx : Positive;
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

                elsif (Is_Array(Card,"PTYPE",1,RANDG_Max,Idx))
                then
                        if(NOT State.PTYPEn(Idx).Read)
                        then
                                State.PTYPEn(Idx).Value := Card(11..30);
                                State.PTYPEn(Idx).Read  := True;
                        else
                                Raise_Exception(Duplicate_Card'Identity, Card);
                        end if;


                 elsif (Is_Array(Card,"PSCAL",1,RANDG_Max,Idx))
                 then
                        if(NOT State.PSCALn(Idx).Read)
                        then
                                State.PSCALn(Idx).Value := Card(11..30);
                                State.PSCALn(Idx).Read  := True;
                        else
                                Raise_Exception(Duplicate_Card'Identity, Card);
                        end if;


                 elsif (Is_Array(Card,"PZERO",1,RANDG_Max,Idx))
                 then
                        if(NOT State.PZEROn(Idx).Read)
                        then
                                State.PZEROn(Idx).Value := Card(11..30);
                                State.PZEROn(Idx).Read  := True;
                        else
                                Raise_Exception(Duplicate_Card'Identity, Card);
                        end if;

		-- Reserved (Primary only)

                elsif (Card(1..8) = "EXTEND  ") then

                        if (NOT State.EXTEND.Read)
                        then
                                State.EXTEND.Value := String(Card(11..30));
                                State.EXTEND.Read  := True;
                        else
                                Raise_Exception(Duplicate_Card'Identity, Card);
                        end if;

                        TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));


		-- Reserved keys (generic)

                elsif( Reserved.Match_Any(m_Options.Reserved,Pos,Card,State.GenRes))
		then
                      TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));

		-- Reserved (generic, data-arryas only)

                elsif( Reserved.Match_Any_DataArr(m_Options.Reserved,Card,State.DataArr))
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
		HDUSizeInfo : HDU_Size_Rec(State.NAXIS_Val);
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
		
		if(m_Options.Mand)
		then

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

		end if;

                return HDUSizeInfo;
        end Get;




-- Priduction keys


  function Needed_Length(Arr : in Prod_Type) return Natural
  is
	  Count : Natural := 0;
  begin
	  for I in Prod_Type'Range
	  loop
		  if(Arr(I).Read) then Count := Count + 1; end if;
	  end loop;

	  return Count;
  end Needed_Length;

  function Get return Prod_Arr
  is
	ArrLen : Natural := Needed_Length(State.GenRes.Prod);
	Arr    : Prod_Arr(1 .. ArrLen);
	Idx : Natural := 1; -- FIXME use Prod_Type'First
  begin
	  for I in Prod_Type'Range
	  loop
		  if(State.GenRes.Prod(I).Read) 
		  then
			 Arr(Idx).Key   := I; 
			 Arr(Idx).Value := State.GenRes.Prod(I).Value; 
			  Idx := Idx + 1; 
		  end if;
	  end loop;

	 return Arr;
  end Get;


	
-- Biblio


  function Needed_Length(Biblio : in Biblio_Type) return Natural
  is
	  Count : Natural := 0;
  begin
	  for I in Biblio_Type'Range
	  loop
		  if(Biblio(I).Read) then Count := Count + 1; end if;
	  end loop;

	  return Count;
  end Needed_Length;

  function Get return Biblio_Arr
  is
	ArrLen : Natural := Needed_Length(State.GenRes.Biblio);
	Arr    : Biblio_Arr(1 .. ArrLen);
	Idx : Natural := 1; -- FIXME use Biblio_Type'First
  begin
	  for I in Biblio_Type'Range
	  loop
		  if(State.GenRes.Biblio(I).Read) 
		  then
			 Arr(Idx).Key   := I; 
			 Arr(Idx).Value := State.GenRes.Biblio(I).Value; 
			  Idx := Idx + 1; 
		  end if;
	  end loop;

	 return Arr;
  end Get;







-- Observational


  function Needed_Length(Obs : in Obs_Type) return Natural
  is
	  Count : Natural := 0;
  begin
	  for I in Obs_Type'Range
	  loop
		  if(Obs(I).Read) then Count := Count + 1; end if;
	  end loop;

	  return Count;
  end Needed_Length;

  function Get return Obs_Arr
  is
	ArrLen : Natural := Needed_Length(State.GenRes.Obs);
	Arr    : Obs_Arr(1 .. ArrLen);
	Idx : Natural := 1; -- FIXME use Obs_Type'First
  begin
	  for I in Obs_Type'Range
	  loop
		  if(State.GenRes.Obs(I).Read) 
		  then
			 Arr(Idx).Key   := I; 
			 Arr(Idx).Value := State.GenRes.Obs(I).Value; 
			  Idx := Idx + 1; 
		  end if;
	  end loop;

	 return Arr;
  end Get;


-- data is Array structure (like IMAGE)

	function Needed_Length(DataArr : Reserved.DataArr_Type) return Natural
	is
		ArrLen : Natural := 0;
	begin
		for I in Reserved.DataArr_Type'Range
		loop
			if(DataArr(I).Read) then ArrLen := ArrLen + 1; end if;
		end loop;

		return ArrLen;
	end Needed_Length;

	function Get return DataArr_Arr
	is
		ArrLen : Natural := Needed_Length(State.DataArr);
		Arr    : DataArr_Arr(1 .. ArrLen);
		Idx    : Natural := 0;
	begin

		for I in Reserved.DataArr_Type'Range
		loop
			if(State.DataArr(I).Read)
			then 
		       		Idx := Idx + 1;	
				Arr(Idx).Key   := I; 
				Arr(Idx).Value := State.DataArr(I).Value; 
			end if;
		end loop;

		return Arr;
	end Get;




end FA_Primary;


