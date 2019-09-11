

-- FIXME not checking for duplicates: if val set already and key arrives check whether the value is the same -  how to do this ??

package body Extension_Mandatory is



type State_Type is (
        UNSPECIFIED,   -- reset was not called yet 
        INITIALIZED,   -- Reset_State was called
	SPECIAL_RECORDS,   -- first key not XTENSION
	CONFORMING,    -- up to GCOUNT all keys present: possible to calculate HDU size. Possibly final state: See NOTE
	IMAGE,         -- final state: CONFORMING and PCOUNT=0 GCOUNT=1
	TABLE,         -- TFIELDS and TFORMn
	BINTABLE,      -- final state: TABLE and XTENSION BINTABLE 
	ASCIITABLE,    -- final state: TCOLn parsed
-- ?	INCONSISTENT_CARDS, -- ERROR if cards for given XTENSION type
        );
	-- NOTE if TABLE arrays broken, instead error transit back to CONFORMING:
	-- we cannot reach the tables of this HDU, but can calculate size, and so reach next HDU
	
State : State_Type := UNSPECIFIED;


        procedure Reset_State
        is
        begin
                MandVals := InitMandVals;
                State := INITIALIZED;
        end Reset_State;


        --
        -- State Transition functions
        --

        procedure In_INITIALIZED
                (Pos  : in Positive;
                Card : in Card_Type)
        is
        begin
                if (Card(1..8) = "XTENSION") then

                        MandVals.XTENSION.Value := String(Card(11..30));
                        MandVals.XTENSION.Read  := True;
                
                        State := CONFORMING;
		else
                        State := SPECIAL_RECORDS;
                end if;

        end In_INITIALIZED;







         procedure In_CONFORMING
                (Pos  : in Positive;
                Card : in Card_Type)
        is
        begin
		if (Card(1..8) = "BITPIX  ") then
                        MandVals.BITPIX.Value := String(Card(11..30));
                        MandVals.BITPIX.Read  := True;

                elsif (Card(1..8) = "NAXIS   ") then
                        MandVals.NAXIS.Value := String(Card(11..30));
                        MandVals.NAXIS.Read := True;

                elsif (Value.Is_Array(Card,"NAXIS",1,NAXIS_Last,Idx)) then
                        MandVals.NAXISn(Idx).Value := String(Card(11..30));
                        MandVals.NAXISn(Idx).Read  := True;

                elsif (Card(1..8) = "PCOUNT  ") then
                        MandVals.PCOUNT.Value := String(Card(11..30));
                        MandVals.PCOUNT.Read  := True;

                elsif (Card(1..8) = "GCOUNT  ") then
                        MandVals.GCOUNT.Value := String(Card(11..30));
                        MandVals.GCOUNT.Read  := True;

                else
                        -- ERROR: unexpected card, non standard or broken Header
                        null;
                end if;

		if(Is_CONFORMING(MandVals)) then 
			-- is conforming if all values BITPIX ... GCOUNT were set

			case(MandVals.XTENSION.Value) is
				when "'IMAGE   '" =>
					State := IMAGE;
				when "'TABLE   '" | "'BINTABLE'" =>
					State := TABLE;
				when others =>
					null;
				-- stay in conforming for non-standard extensions
				-- e.g. XTENSION calue none of three IMAGE BINTABLE ASCIITABLE
			end case;

		end if;

        end In_CONFORMING;




        procedure In_TABLE
                (Pos  : in Positive;
                Card : in Card_Type)
        is
        begin
                if (Card(1..8) = "TFIELDS ") then
                        MandVals.NAXIS.Value := String(Card(11..30));
                        MandVals.NAXIS.Read := True;

                elsif (Value.Is_Array(Card,"TFORM",1,NAXIS_Last,Idx)) then
                        MandVals.NAXISn(Idx).Value := String(Card(11..30));
                        MandVals.NAXISn(Idx).Read  := True;

                else
                        -- ERROR: unexpected card, non standard or broken Header
                        null;
                end if;


		if(Is_TFORM_Set(MandVals) AND MabdVals.XTENSION.Value = "'TABLE   '") then
			State := ASCIITABLE;
		else
			State := BINTABLE;
		end if;


        end In_TABLE;


      procedure In_ASCIITABLE
                (Pos  : in Positive;
                Card : in Card_Type)
        is
        begin
                if (Value.Is_Array(Card,"TBCOL",1,NAXIS_Last,Idx)) then
                        MandVals.NAXISn(Idx).Value := String(Card(11..30));
                        MandVals.NAXISn(Idx).Read  := True;

                else
                        -- ERROR: unexpected card, non standard or broken Header
                        null;
                end if;

        end In_ASCIITABLE;








	--
	-- State Machine
	--

        function  Next
                (Pos  : in Positive; 
                 Card : in Card_Type) return Read_Control
        is
                Rc : Read_Control := Continue;
        begin
                if( (NOT(State = UNSPECIFIED)) 
                     AND (Card = ENDCard) ) then

                        MandVals.ENDCardPos := Pos;
                        MandVals.ENDCardSet := True;
                        Rc := Stop;
                        
                        DBG_Print;
                end if;
                        
                case(State) is
                        when INITIALIZED =>
                               In_INITIALIZED(Pos, Card);
                        when SPECIAL_RECORDS =>
                                null;
                        when CONFORMING => 
                               In_CONFORMING(Pos, Card);
                        when IMAGE => -- a standard extension 
                               null; 
                        when TABLE =>
                               In_TABLE(Pos, Card);     
                        when BINTABLE => -- a standard extension
                               null; 
                        when ASCIITABLE => -- a standard extension
                               In_ASCIITABLE(Pos, Card);     
                        when others => 
                                -- including UNSPECIFIED
                                -- programming error: Reset_State 
                                -- must be called before New_Card
                                null;
                end case;
                
                return Rc;

        end Next;


end Extension_Mandatory;


