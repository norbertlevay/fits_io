



package body Primary_Strict is


Keys : array (1 .. <>) of String(1..8) := 
	("SIMPLE  ", "BITPIX  ", "NAXIS   ", "NAXIS   ");

type State_Type is 
	(UNSPECIFIED, SCALAR_KEY, COLLECT_ARRAY, WAIT_END);





        procedure Reset_State
        is
        begin
                MandVals := InitMandVals;
                State := SCALAR_KEY;
        end Reset_State;




        --
        -- State Transition functions
        --

         procedure In_SCALAR_KEY
                (Pos  : in Positive;
                Card : in Card_Type)
        is
        begin
		RefPos := Pos - Offset;

                if (Card(1..8) = Keys(RefPos)) then

			-- store the value

                        MandVals(RefPos).Value := String(Card(11..30));
                        MandVals(RefPes).Read  := True;

			-- check if state change needed

			case(RefPos) is
				when 3 => -- NAXIS
	                       		if(Value = ZeroValue) -- NAXIS = 0
        	                	then
                        	       		State := WAIT_END;
					end if;

                        	when 4 => -- NAXISn (and NAXIS /= 0)
				
					Arr_Root := Keys(RefPos);
					Arr_Last := Positive'Image(MandVals(3).Value);-- NAXIS
                                	State := COLLECT_ARRAY;

				when 8 => -- GCOUNT
					State := WAIT_END; 

				when others =>
					null;
			end case;
	
                else
                        -- ERROR: unexpected card, non standard or broken Header
                        null;
                end if;

        end In_SCALAR_KEY;


       procedure In_COLLECT_ARRAY
                (Pos  : in Positive;
                 Card : in Card_Type)
        is
        begin
		-- FIXME unite the two ifs by similar to Is_Array(Pos, Card, Root, Idx)
                if (Card(1..Arr_Root'Last) = Arr_Root) then
			Idx := Get_Index(Card(1..8), Arr_Root);
			if ( (Idx = (Pos - 3)) AND (Idx <= Arr_Last)) then
	                       
				-- store the value

				MandVals.NAXISn(Idx).Value := String(Card(11..30));
        	                MandVals.NAXISn(Idx).Read  := True;

				-- check if state change needed

				if(Arr_Last = Idx) -- last array element was stored
        	                then
					Offset := Offset + Arr_Last;

					if(MandVals.NAXISn(1).Value = ZeroValue) then
                	                	State := SCALAR_KEY; -- still read GROUPS PCOUNT GCOUNT
					else
						State := WAIT_END;
					end if;
                        	end if;
			else 
				-- ERROR: unexpected card at Pos
				null;	
			end if;
                else
                        -- ERROR: unexpected card at Pos
                        null;
                end if;

        end In_COLLECT_ARRAY;



         procedure In_WAIT_END
                (Pos  : in Positive;
                Card : in Card_Type)
        is
        begin
               if (Card = ENDCard) then

                        MandVals.ENDCardPos := Pos;
                        MandVals.ENDCardSet := True;
                        Rc := Stop;

                        DBG_Print;
                end if;

	end In_WAIT_END;
	








        function  Next
                (Pos  : in Positive;
                 Card : in Card_Type) return Read_Control
        is
                Rc : Read_Control := Continue;
        begin

                case(State) is
                        when SCALAR_KEY =>
                               In_SCALAR_KEY(Pos, Card);
                        when COLLECT_ARRAY =>
                               In_COLLECT_ARRAY(Pos, Card);
                        when WAIT_END =>
                               In_WAIT_END(Pos, Card);
                        when others =>
                                -- including UNSPECIFIED
                                -- programming error: Reset_State 
                                -- must be called before New_Card
                                null;
                end case;

                return Rc;

        end Next;





end Primary_Strict;

