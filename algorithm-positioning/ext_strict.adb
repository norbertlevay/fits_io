


package body Ext_Strict is


	

RefKeys : array (Positive 1..<>) of String(1..8) := (
	"XTENSION", 
	"BITPIX  ",
	"NAXIS   ",
	"NAXIS   ",
	"PCOUNT  ",
	"GCOUNT  ",
	"TFIELDS ",
	"TFORM   ",
	"TBCOL   "
		);
	

Vals : array (Positive 1..RefKeys'Last) of String(1..20);

NAXISn : array (Positive 1.. NAXIS_Last) of String(1..20);
TFORMn : array (Positive 1.. NAXIS_Last) of String(1..20);
TBCOLn : array (Positive 1.. NAXIS_Last) of String(1..20);


type State_Type is (UNSPECIFIED, INITIALIZED, RAND_BLOCKS, 
	READ_SCALAR, 
	COLLECT_NAXIS_ARRAY, COLLECT_TFORM_ARRAY, COLLECT_TBCOL_ARRAY,
	WAIT_END);

type State_Rec is 
	record
		State : State_Type;
		XTENSION : String(1..20);--value
		Arr_Root : String;
		Arr_Last : Positive;
		Offset   : Natural;
	end record;

StateRec : State_Rec := (State => UNSPECIFIED, Offset => 0);


	procedure Reset_State 
	is
	begin
		StateRec.State  := INITIALIZED;
		StateRec.Offset := 0;
	end Reset_State;




	function In_INITIALIZED(RefPos : Positive; Card : Card_Type) return Positive
	is
	begin
		if( RefKeys(RefPos) = Card(1..8) ) then
			Vals(RefPos)   := Card(11.30);
			StateRec.State := READ_SCALAR;

		elsif("        " = Card(1..8)) then
			StateRec.State := RAND_BLOCKS;

		else
			-- ERROR 
			null;
		end if;

		return RefPos + 1;

	end In_INITIALIZED;


	function In_READ_SCALAR(RefPos : Positive; Card : Card_Type) return Positive
	is
	begin
		case(RefPos) is
			when 2 .. 3, 5, 7 =>
				if ( RefKeys(RefPos) = Card(1..8) ) then
					Vals(RefPos) := Card(11..30);
				else
					-- ERROR unexpected card
					null;
				end if;
	
			when 4 =>
				if ( Is_Match(RefKeys(RefPos),"NAXIS",NAXIS_Last) ) then
					StateRec.Arr_Root := "NAXIS";
					StateRec.Arr_Last := Positive'Image(Vals(3));
					StateRec.State := COLLECT_ARRAY;
				else
					-- ERROR unexpected card
					null;
				end if;

			when 6 =>
				if ( RefKeys(RefPos) = Card(1..8) ) then
					Vals(RefPos) := Card(11..30);
					if(Vals(1) = "IMAGE   ") then
						StateRec.State = WAIT_END;
					end if;
				else
					-- ERROR unexpected card
					null;
				end if;

			when 8 => 
                               if ( Is_Match(RefKeys(RefPos),"TFORM",NAXIS_Last) ) then
                                        StateRec.Arr_Root := "TFORM";
                                        StateRec.Arr_Last := Positive'Image(Vals(7));
                                        StateRec.State := COLLECT_ARRAY;
                                else
                                        -- ERROR unexpected card
                                        null;
                                end if;


			when 9 => 
                               if ( Is_Match(RefKeys(RefPos),"TBCOL",NAXIS_Last) ) then
                                        StateRec.Arr_Root := "TBCOL";
                                        StateRec.Arr_Last := Positive'Image(Vals(7));
                                        StateRec.State := COLLECT_ARRAY;
                                else
                                        -- ERROR unexpected card
                                        null;
                                end if;

		end case;
	end READ_SCALAR;
	




	function Next (Pos : Positive;
		Card : Card_Type) return Positive
	is
		RefPos : Positive;
	begin
		RefPos := Pos - StateRec.Offset;

		case(StateRec.State) is
			when UNSPECIFIED => 
				null;
			when INITIALIZED => 
				In_INITIALIZED(RefPos, Card);
			when RAND_BLOCKS =>
				null;
			when READ_SCALAR =>
				In_READ_SCALAR(RefPos, Card);
			when COLLECT_ARRAY =>
				In_COLLECT_ARRAY(RefPos, Card);
			when WAIT_END =>
				In_WAIT_END(RefPos, Card);
	end Next;








end Ext_Strict;


