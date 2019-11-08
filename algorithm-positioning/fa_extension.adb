with Ada.Text_IO; use Ada.Text_IO; -- for debug only DBG_Print, Trace_State

with Ada.Exceptions; use Ada.Exceptions;

with FITS; use FITS; -- Card_Type needed
with Keyword_Record; use Keyword_Record;
with Reserved;


package body FA_Extension is


m_Options : Options_Type := (others => False);


	--
        -- definition of states
        --

type State_Name is
        (NOT_ACCEPTING_CARDS,  	-- FA inactive
         IS_CONFORMING, 	-- Initial state
         COLLECT_TABLE_ARRAYS, 	-- collect TFORM & TBCOL arrays and END-card
         WAIT_END,             	-- ignore all cards except END-card
	 SPECIAL_RECORDS,	-- Final state (not XTENSION)
	 CONFORMING_EXTENSION,	-- Final state (is XTENSION bun not Standard)
	 IMAGE, TABLE, BINTABLE -- Final states (Standard Ext)
	 );	


type NAXIS_MaxArr   is array (1..NAXIS_Max)   of CardValue;

InitNAXISArrVal : constant NAXIS_MaxArr := (others => InitVal);
InitTFORMArrVal : constant TFIELDS_Arr  := (others => InitVal);
InitTBCOLArrVal : constant TFIELDS_Arr  := (others => InitVal);

-- Reserved (optional) keys in any Conforming Extension
type ConfExt_Type is
        record
                EXTNAME  : CardValue;
                EXTVER   : CardValue;
                EXTLEVEL : CardValue;
        end record;

InitConfExt : ConfExt_Type := (InitVal, InitVal, InitVal);

-- Reserved (optional) keys related to TABLEs
type Tab_Type is
        record
                TTYPEn : TFIELDS_Arr;
                TUNITn : TFIELDS_Arr;
                TSCALn : TFIELDS_Arr;
                TZEROn : TFIELDS_Arr;
                TNULLn : TFIELDS_Arr;
                TDISPn : TFIELDS_Arr;
        end record;

InitTFIELDSArr : constant TFIELDS_Arr := (others => InitVal);

InitTab : Tab_Type := (others => InitTFIELDSArr);

type BinTab_Type is
	record
		TDIMn : TFIELDS_Arr;
		THEAP : CardValue;
	end record;

InitBinTab : BinTab_Type := (InitTFIELDSArr, InitVal);


type XT_Type is
        (UNSPECIFIED, IMAGE, ASCII_TABLE, BIN_TABLE);

type State_Type is
        record
	PrevPos : Natural;

        Name         : State_Name;
        XTENSION_Val : XT_Type;
        NAXIS_Val    : Natural;
        TFIELDS_Val  : Natural;

	-- Mandatory
        XTENSION : CardValue;
        BITPIX   : CardValue;
        NAXIS    : CardValue;
        NAXISn   : NAXIS_MaxArr;
        PCOUNT   : CardValue;
        GCOUNT   : CardValue;
        TFIELDS  : CardValue;
        TFORMn   : TFIELDS_Arr;
        TBCOLn   : TFIELDS_Arr;
	
	-- Reserved
	Res : Reserved.Extension_Type;

	-- other cards not recognized by this FA
        OtherCount : Natural;

        ENDCardPos : Natural;
        ENDCardSet : Boolean;
        end record;

InitState : State_Type := 
	(
	0,
	NOT_ACCEPTING_CARDS, UNSPECIFIED, 0, 0, 
	
	InitVal,InitVal,InitVal,
        InitNAXISArrVal,
        InitVal,InitVal,InitVal,
        InitTFORMArrVal,
        InitTBCOLArrVal,
	Reserved.ExtInit,
	0,
        0,False);

State : State_Type := InitState;
------------------------------------------------------------------
package TIO renames Ada.Text_IO;

procedure DBG_Print(BinTab : BinTab_Type) 
is
begin

TIO.Put("TDIM: ");
for I in BinTab.TDIMn'Range
loop
	if(BinTab.TDIMn(I).Read) then Put(Positive'Image(I) &":"& BinTab.TDIMn(I).Value & " "); end if;
end loop;
New_Line;
if(BinTab.THEAP.Read) then TIO.Put_Line("BinTab THEAP "&BinTab.THEAP.Value); end if;
end DBG_Print;

procedure DBG_Print(Tab : Tab_Type) 
is
begin
TIO.Put("TSCAL: ");
for I in Tab.TSCALn'Range
loop
	if(Tab.TSCALn(I).Read) then Put(Positive'Image(I) &":"& Tab.TSCALn(I).Value & " "); end if;
end loop;
New_Line;
TIO.Put("TZERO: ");
for I in Tab.TZEROn'Range
loop
	if(Tab.TZEROn(I).Read) then Put(Positive'Image(I) &":"& Tab.TZEROn(I).Value & " "); end if;
end loop;
New_Line;
TIO.Put("TNULL: ");
for I in Tab.TNULLn'Range
loop
	if(Tab.TNULLn(I).Read) then Put(Positive'Image(I) &":"& Tab.TNULLn(I).Value & " "); end if;
end loop;
New_Line;
TIO.Put("TTYPE: ");
for I in Tab.TTYPEn'Range
loop
	if(Tab.TTYPEn(I).Read) then Put(Positive'Image(I) &":"& Tab.TTYPEn(I).Value & " "); end if;
end loop;
New_Line;
TIO.Put("TUNIT: ");
for I in Tab.TUNITn'Range
loop
	if(Tab.TUNITn(I).Read) then Put(Positive'Image(I) &":"& Tab.TUNITn(I).Value & " "); end if;
end loop;
New_Line;
TIO.Put("TDISP: ");
for I in Tab.TDISPn'Range
loop
	if(Tab.TDISPn(I).Read) then Put(Positive'Image(I) &":"& Tab.TDISPn(I).Value & " "); end if;
end loop;
New_Line;
end DBG_Print;

	
procedure DBG_Print(IdxKeys : IdxKey_Rec_Arr)
is
begin
	TIO.Put_Line("DBG_Print IdxKeys");
	for I in IdxKeys'Range
	loop
		TIO.Put("Get ResArr "& Root_Type'Image(IdxKeys(I).Root)&" ");
		for Idx in IdxKeys(I).Arr'Range
		loop
			TIO.Put(Integer'Image(Idx) &":"& IdxKeys(I).Arr(Idx).Value);
		end loop;
		TIO.New_Line;
	end loop;
end DBG_Print;


procedure DBG_Print
is
begin
TIO.New_Line;
if(State.XTENSION.Read) then TIO.Put_Line("XTENSION "&State.XTENSION.Value); end if;
if(State.BITPIX.Read)   then TIO.Put_Line("BITPITX  "&State.BITPIX.Value);   end if;
if(State.NAXIS.Read)    then TIO.Put_Line("NAXIS    "&State.NAXIS.Value);    end if;
TIO.Put("NAXIS: ");
for I in State.NAXISn'Range
loop
 if(State.NAXISn(I).Read) then Put(Positive'Image(I) &":"& State.NAXISn(I).Value & " "); end if;
end loop;
New_Line;
if(State.PCOUNT.Read)  then TIO.Put_Line("PCOUNT  "&State.PCOUNT.Value);  end if;
if(State.GCOUNT.Read)  then TIO.Put_Line("GCOUNT  "&State.GCOUNT.Value);  end if;
if(State.TFIELDS.Read) then TIO.Put_Line("TFIELDS "&State.TFIELDS.Value); end if;
TIO.Put("TFORM: ");
for I in State.TFORMn'Range
loop
	if(State.TFORMn(I).Read) then Put(Positive'Image(I) &":"& State.TFORMn(I).Value & " "); end if;
end loop;
New_Line;
TIO.Put("TBCOL: ");
for I in State.TBCOLn'Range
loop
	if(State.TBCOLn(I).Read) then Put(Positive'Image(I) &":"& State.TBCOLn(I).Value & " "); end if;
end loop;
New_Line;
Reserved.DBG_Print(State.Res);
TIO.Put_Line(State_Name'Image(State.Name));
end DBG_Print;


procedure DBG_Print_Reserved
is
        Res : Key_Rec_Arr := Get((EXTNAME,DATAMAX,DATAMIN,INSTRUME,TELESCOP));
begin
	TIO.Put_Line("DBG_Print_reserved");
        for I in Res'Range
        loop
                TIO.Put("Get Res> "&Reserved_Key'Image(Res(I).Key));
                TIO.Put(" : "&Res(I).Value);
                TIO.New_Line;
        end loop;


end DBG_Print_Reserved;





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
		State      := InitState;
		-- FIXME m_Options is incorrect
		
                TIO.Put_Line("Opts: "
                &" "&Boolean'Image(m_Options.Mand) 
                &" "&Boolean'Image(m_Options.Tab)
                );
		
                if(m_Options.Mand)
                then
                        State.Name := IS_CONFORMING;

		elsif(m_Options.Tab)
                then
                        State.Name := COLLECT_TABLE_ARRAYS;

		else
                        State.Name := WAIT_END;
                end if;
		return 1; -- start FA from Header's 1st card	
	end Reset_State;




	function In_IS_CONFORMING(Pos : Positive; Card : Card_Type) return Positive
	is
		Idx : Positive;
	begin
		TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));
		if(Pos = 1)
		then
			-- [FITS 3.5] The first 8 bytes of the special records 
			-- must not contain the string “XTENSION”.
			if( "XTENSION" = Card(1..8) )
			then
				State.XTENSION.Value := Card(11..30);
				State.XTENSION.Read  := True;
				State.XTENSION_Val := To_XT_Type(State.XTENSION.Value);
			else
				State.Name := SPECIAL_RECORDS;
			end if;

		elsif    ( "BITPIX  " = Card(1..8) AND (Pos = 2) )
		then
			State.BITPIX.Value := Card(11..30);
			State.BITPIX.Read  := True;

		elsif ( "NAXIS   " = Card(1..8) AND (Pos = 3) )
		then
			State.NAXIS.Value := Card(11..30);
			State.NAXIS.Read  := True;

			State.NAXIS_Val      := To_Integer(State.NAXIS.Value);

		elsif ( Is_Array(Card,"NAXIS",1,NAXIS_Max,Idx) )
			-- FIXME NAXIS_Max should be NAXIS_Val but how to 
			-- guarantee it is read before used ? The same for Idx: nested if's !!
		then
			if(Pos = 3 + Idx)
			then
				State.NAXISn(Idx).Value := Card(11..30);
				State.NAXISn(Idx).Read  := True;
			else
				Raise_Exception(Unexpected_Card'Identity, Card);
			end if;
	
		elsif ( "PCOUNT  " = Card(1..8) AND (Pos = 3 + State.NAXIS_Val + 1))
		then
			State.PCOUNT.Value := Card(11..30);
			State.PCOUNT.Read  := True;

		elsif ( "GCOUNT  " = Card(1..8) AND (Pos = 3 + State.NAXIS_Val + 2))
		then
			State.GCOUNT.Value := Card(11..30);
			State.GCOUNT.Read  := True;

			case(State.XTENSION_Val) is
				when IMAGE  => State.Name := WAIT_END;
				when ASCII_TABLE | BIN_TABLE => null;
				when others => State.Name := WAIT_END;
			end case;

		elsif ("TFIELDS " = Card(1..8) AND (Pos = 3 + State.NAXIS_Val + 3) )
		then
			State.TFIELDS.Value := Card(11..30);
			State.TFIELDS.Read  := True;

			State.TFIELDS_Val := To_Integer(State.TFIELDS.Value);

			case(State.XTENSION_Val) is
				when ASCII_TABLE | BIN_TABLE => State.Name := COLLECT_TABLE_ARRAYS;
				when others => 
					Raise_Exception(Unexpected_Card'Identity, Card);
			end case;

		else
			Raise_Exception(Unexpected_Card'Identity, Card);
		end if;

		return Pos + 1;

	end In_IS_CONFORMING;




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
		-- Reserved (all Conforming Extensions)

		if ( Reserved.Match_Any_ConfExt(Card, State.Res.Ext) )
		--if ( Match_ConfExt(Card, State.ConfExt) )
		then
			TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));

		-- Reserved (generic)

                elsif( Reserved.Match_Any(m_Options.Reserved,Pos,Card,State.Res))
                then
                      TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));

                -- Reserved (generic, image-like only)

                elsif( Reserved.Match_Any_DataArr(m_Options.Reserved,Card,State.Res.Comm))
                then
                      TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));



		elsif( ENDCard = Card ) then
			State.ENDCardPos := Pos;
			State.ENDCardSet := True;

			TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));
			
                        if( m_Options.Mand )
                        then
        			case(State.XTENSION_Val) is
				when IMAGE  => State.Name := IMAGE;
				when ASCII_TABLE | BIN_TABLE => null;
				when others => State.Name := CONFORMING_EXTENSION;
				end case;
                        State.Name := IMAGE;
                        else
                                null;-- FIXME State.Name := NOT_DETERMINED;
		        end if;
			return 0;
			-- no more cards


                elsif(Is_Fixed_Position(Card)) then
                        -- one of IS_CONFORMING cards: may appear only once in header
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


	-- NOTE Ada2005 has package Ada.Assertions; and also 'pragma Assert()'
  	procedure Assert_Array_Complete(ArrName : String; Length : Positive; Arr : TFIELDS_Arr )
	is
		ArrComplete : Boolean := True;
	begin
		for Ix in 1 .. Length 
		loop
			if(Arr(Ix).Read)
			then
				null;
			else
				Raise_Exception(Card_Not_Found'Identity, ArrName&Integer'Image(Ix));
			end if;
		end loop;
	end Assert_Array_Complete;



	function Match_Any_Tab(Card : in Card_Type;
				Tab : in out Tab_Type) return Boolean
	is
		Ix : Positive;
	begin
		if(NOT m_Options.Tab) 
		then
			return False;
		end if;


		if ( "TSCAL" = Card(1..5) )
                then
                        Ix := Extract_Index("TSCAL",Card(1..8));
                        if(NOT Tab.TSCALn(Ix).Read)
                        then
                                Tab.TSCALn(Ix).Value := Card(11..30);
                                Tab.TSCALn(Ix).Read := True;
        		else
                                Raise_Exception(Duplicate_Card'Identity, Card);
			end if;

		elsif ( "TZERO" = Card(1..5) )
                then
                        Ix := Extract_Index("TZERO",Card(1..8));
                        if(NOT Tab.TZEROn(Ix).Read)
                        then
                                Tab.TZEROn(Ix).Value := Card(11..30);
                                Tab.TZEROn(Ix).Read := True;
        		else
                                Raise_Exception(Duplicate_Card'Identity, Card);
			end if;
		
		elsif ( "TNULL" = Card(1..5) )
                then
                        Ix := Extract_Index("TNULL",Card(1..8));
                        if(NOT Tab.TNULLn(Ix).Read)
                        then
                                Tab.TNULLn(Ix).Value := Card(11..30);
                                Tab.TNULLn(Ix).Read := True;
        		else
                                Raise_Exception(Duplicate_Card'Identity, Card);
			end if;

                elsif ( "TTYPE" = Card(1..5) )
                then
                        Ix := Extract_Index("TTYPE",Card(1..8));
                        if(NOT Tab.TTYPEn(Ix).Read)
                        then
                                Tab.TTYPEn(Ix).Value := Card(11..30);
                                Tab.TTYPEn(Ix).Read := True;
        		else
                                Raise_Exception(Duplicate_Card'Identity, Card);
			end if;

		elsif ( "TUNIT" = Card(1..5) )
                then
                        Ix := Extract_Index("TUNIT",Card(1..8));
                        if(NOT Tab.TUNITn(Ix).Read)
                        then
                                Tab.TUNITn(Ix).Value := Card(11..30);
                                Tab.TUNITn(Ix).Read := True;
	       		else
                                Raise_Exception(Duplicate_Card'Identity, Card);
			end if;

		elsif ( "TDISP" = Card(1..5) )
                then
                        Ix := Extract_Index("TDISP",Card(1..8));
                        if(NOT Tab.TDISPn(Ix).Read)
                        then
                                Tab.TDISPn(Ix).Value := Card(11..30);
                                Tab.TDISPn(Ix).Read := True;
	       		else
                                Raise_Exception(Duplicate_Card'Identity, Card);
			end if;


		else
			return False;
		end if;

		return True;
   
	end Match_Any_Tab;



	function In_COLLECT_TABLE_ARRAYS(Pos : Positive; Card : Card_Type) return Natural
	is
		Ix : Positive := 1;
	begin

 		-- Mandatory cards

		if ( "TFORM" = Card(1..5) )
		then
			Ix := Extract_Index("TFORM",Card(1..8));
			if(NOT State.TFORMn(Ix).Read)
			then
				State.TFORMn(Ix).Value := Card(11..30);
				State.TFORMn(Ix).Read := True;
			else
				-- FIXME only duplicates with diff values raises exception
				-- duplicate with equal values: make configurable what to do...
				Raise_Exception(Duplicate_Card'Identity, Card);
			end if;
			
			TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));

		elsif ( "TBCOL" = Card(1..5) )
		then
			if(State.XTENSION_Val = ASCII_TABLE) 
			then
				Ix := Extract_Index("TBCOL",Card(1..8));
				if(NOT State.TBCOLn(Ix).Read)
				then
					State.TBCOLn(Ix).Value := Card(11..30);
					State.TBCOLn(Ix).Read  := True;
				else
	                                Raise_Exception(Duplicate_Card'Identity, Card);
                	        end if;

				TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));
			else
				Raise_Exception(Unexpected_Card'Identity, Card);
			end if;

		-- Reserved (all Conforming Extensions)

		elsif ( Reserved.Match_Any_ConfExt(Card, State.Res.Ext) )
		then
			TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));

		-- Reserved (TABLE or BINTABLE specific)

		elsif ( Reserved.Match_Any_Tab(Card, State.Res.Arr) )
		then
			TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));

		elsif ( Reserved.Match_Any_BinTab(Card, State.Res) )
		then
			TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));

		-- Reserved (generic)

		elsif( Reserved.Match_Any(m_Options.Reserved,Pos,Card,State.Res))
                then
                      TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));


		elsif( Card = ENDCard )
		then
			State.ENDCardPos := Pos;
			State.ENDCardSet := True;

			TIO.Put_Line(State_Name'Image(State.Name)&"::"&Card(1..8));

                        case(State.XTENSION_Val) is
                               	when ASCII_TABLE => 
  					Assert_Array_Complete("TFORM",State.TFIELDS_Val, State.TFORMn);
  					Assert_Array_Complete("TBCOL",State.TFIELDS_Val, State.TBCOLn);
					State.Name := TABLE;
					return 0;
					-- no more cards

                               	when BIN_TABLE   => 
  					Assert_Array_Complete("TFORM",State.TFIELDS_Val, State.TFORMn);
					State.Name := BINTABLE;
					return 0;
					-- no more cards

                               	when others => null;
			end case;


		elsif(Is_Fixed_Position(Card))
		then
                        Raise_Exception(Duplicate_Card'Identity, Card);
                        -- one of IS_CONFORMING cards: may appear only once in header

                elsif(Is_Valid(Card)) then
                        -- valid card defined by [FITS Appendix A] BNF syntax
                        State.OtherCount := State.OtherCount + 1;
                        -- valid but unknown to this FA-implementation
                else
                        Raise_Exception(Invalid_Card'Identity, Card);
                        -- found card which does not confirm [FITS Addendix A] BNF syntax
		end if;

                return Pos + 1;

	end In_COLLECT_TABLE_ARRAYS;
	





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

			when IS_CONFORMING =>
				NextCardPos := In_IS_CONFORMING(Pos, Card);
			when WAIT_END =>
				NextCardPos := In_WAIT_END(Pos, Card);
			when COLLECT_TABLE_ARRAYS =>
				NextCardPos := In_COLLECT_TABLE_ARRAYS(Pos, Card);

			when SPECIAL_RECORDS | CONFORMING_EXTENSION 
			     | IMAGE | TABLE |BINTABLE =>
				NextCardPos := 0;
		end case;
		
		if(NextCardPos = 0) then DBG_Print; end if;
		if(NextCardPos = 0) then DBG_Print_Reserved; end if;
		if(NextCardPos = 0) then DBG_Print(Get((TSCAL,TTYPE,TUNIT,TDISP))); end if;

		return NextCardPos;

	end Next;






-- read Mandatory keys as record

        function To_Extension_HDU(StateName : in FA_Extension.State_Name) return Extension_HDU
        is
                t : Extension_HDU;
        begin
                case(StateName) is
                        when SPECIAL_RECORDS 	=> t := SPECIAL_RECORDS;
                        when CONFORMING_EXTENSION => t := CONFORMING_EXTENSION;
                        when IMAGE		=> t := STANDARD_IMAGE;
                        when TABLE		=> t := STANDARD_TABLE;
                        when BINTABLE		=> t := STANDARD_BINTABLE;
                        when others =>
                                Raise_Exception(Programming_Error'Identity,
                                "Not all cards read. State "&
                                FA_Extension.State_Name'Image(StateName) );
                end case;

                return t;

        end To_Extension_HDU;



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
                
			-- FIXME how about XTENSION value, shoule we check it was set ?

	                HDUSizeInfo.HDUType := To_Extension_HDU(State.Name);

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

-- FIXME here? to check whether XTENSION typ vs P/GCOUNT values match ?

			if(State.PCOUNT.Read) then
        	                HDUSizeInfo.PCOUNT := To_Integer(State.PCOUNT.Value);
               		else
                        	Raise_Exception(Card_Not_Found'Identity, "PCOUNT");
	                end if;

			if(State.GCOUNT.Read) then
        	                HDUSizeInfo.GCOUNT := To_Integer(State.GCOUNT.Value);
               		else
                        	Raise_Exception(Card_Not_Found'Identity, "GCOUNT");
	                end if;

		end if;

                return HDUSizeInfo;
        end Get;



-- read reserved scalar keys

 function Needed_Count(Keys : in Res_Key_Arr) return Natural
 is
         Count : Natural := 0;
 begin

 	 for I in Keys'Range
	 loop
		 case(Keys(I)) is
			when DATE .. DATAMIN =>

        	        if(State.Res.Comm(Keys(I)).Read)
                	then
                        	Count := Count + 1;
             		end if;


			when EXTNAME .. THEAP =>

			TIO.Put(Reserved_Key'Image(Keys(I)) & " ");
                	if(State.Res.Ext(Keys(I)).Read)
                	then
                        	Count := Count + 1;
                	end if;

--			when others =>	null;
		end case;
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
		case(Keys(I)) is
			when DATE .. DATAMIN =>

        	        if(State.Res.Comm(Keys(I)).Read)
                	then
            	            OutKeys(Idx).Key   := Keys(I);
                	    OutKeys(Idx).Value := State.Res.Comm(Keys(I)).Value;
                            exit when (Idx = FoundCount);
                   	    Idx := Idx + 1;
                	end if;

		when EXTNAME .. THEAP =>

			if(State.Res.Ext(Keys(I)).Read)
                	then
                        	OutKeys(Idx).Key   := Keys(I);
	                        OutKeys(Idx).Value := State.Res.Ext(Keys(I)).Value;
        	                exit when (Idx = FoundCount);
                	        Idx := Idx + 1;
        	        end if;

--			when others => null;
		end case;

        end loop;

        return OutKeys;
 end Get;


	-- experimental Get(RootArr) to return Tab_Type's read arrays

	-- FIXME modify so that Arr can be shorter, containing only the Read elements
	function Is_Any_Element_Read(Arr : TFIELDS_Arr) return Boolean
	--function Is_Any_Element_Read(Arr : Reserved.TFIELDS_Arr) return Boolean
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
		if(Is_In_Set(TTYPE,Roots)) then
			if(Is_Any_Element_Read(State.Res.Arr(Reserved.TTYPE))) then Len := Len + 1; end if;
		end if;
		if(Is_In_Set(TUNIT,Roots)) then
			if(Is_Any_Element_Read(State.Res.Arr(Reserved.TUNIT))) then Len := Len + 1; end if;
		end if;
		if(Is_In_Set(TSCAL,Roots)) then
			if(Is_Any_Element_Read(State.Res.Arr(Reserved.TSCAL))) then Len := Len + 1; end if;
		end if;
		if(Is_In_Set(TZERO,Roots)) then
			if(Is_Any_Element_Read(State.Res.Arr(Reserved.TZERO))) then Len := Len + 1; end if;
		end if;
		if(Is_In_Set(TNULL,Roots)) then
			if(Is_Any_Element_Read(State.Res.Arr(Reserved.TNULL))) then Len := Len + 1; end if;
		end if;
		if(Is_In_Set(TDISP,Roots)) then
			if(Is_Any_Element_Read(State.Res.Arr(Reserved.TDISP))) then Len := Len + 1; end if;
		end if;
		return Len;
	end Needed_Length;


	function Get(Roots : Root_Arr) return IdxKey_Rec_Arr
	is
		IdxKey : IdxKey_Rec_Arr(1..Needed_Length(Roots));
		Len : Natural := 0;-- FIXME rename to Idx
	begin
		if(Is_In_Set(TTYPE,Roots))
		then
			if(Is_Any_Element_Read(State.Res.Arr(Reserved.TTYPE)))
			then 
				Len := Len + 1;
				IdxKey(Len).Root := TTYPE;
				IdxKey(Len).Arr  := State.Res.Arr(Reserved.TTYPE);
			end if;
		end if;

		if(Is_In_Set(TUNIT,Roots))
		then
			if(Is_Any_Element_Read(State.Res.Arr(Reserved.TUNIT)))
			then 
				Len := Len + 1;
				IdxKey(Len).Root := TUNIT;
				IdxKey(Len).Arr  := State.Res.Arr(Reserved.TUNIT);
			end if;
		end if;

		if(Is_In_Set(TSCAL,Roots))
		then
			if(Is_Any_Element_Read(State.Res.Arr(Reserved.TSCAL)))
			then 
				Len := Len + 1;
				IdxKey(Len).Root := TSCAL;
				IdxKey(Len).Arr  := State.Res.Arr(Reserved.TSCAL);
			end if;
		end if;

		if(Is_In_Set(TZERO,Roots))
		then
			if(Is_Any_Element_Read(State.Res.Arr(Reserved.TZERO)))
			then 
				Len := Len + 1;
				IdxKey(Len).Root := TZERO;
				IdxKey(Len).Arr  := State.Res.Arr(Reserved.TZERO);
			end if;
		end if;

		if(Is_In_Set(TNULL,Roots))
		then
			if(Is_Any_Element_Read(State.Res.Arr(Reserved.TNULL)))
			then 
				Len := Len + 1;
				IdxKey(Len).Root := TNULL;
				IdxKey(Len).Arr  := State.Res.Arr(Reserved.TNULL);
			end if;
		end if;

		if(Is_In_Set(TDISP,Roots))
		then
			if(Is_Any_Element_Read(State.Res.Arr(Reserved.TDISP)))
			then 
				Len := Len + 1;
				IdxKey(Len).Root := TDISP;
				IdxKey(Len).Arr  := State.Res.Arr(Reserved.TDISP);
			end if;
		end if;

	return IdxKey;
	end Get;


end FA_Extension;



