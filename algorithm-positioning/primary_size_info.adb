
with Ada.Text_IO; -- for debug only DBG_Print
with Value;

package body Primary_Size_Info is


EmptyVal : constant String(1..20) := (others => ' ');

type CardValue is
        record
                Value : String(1..20);
                Read  : Boolean;
        end record;
InitVal  : constant CardValue := (EmptyVal,False);

type NAXISn_Arr is array (1..NAXIS_Last) of CardValue;
InitNAXISArrVal : constant NAXISn_Arr := (others => InitVal);

type Primary_Mandatory_Card_Values is
        record
        SIMPLE : CardValue;
        BITPIX : CardValue;
        NAXIS  : CardValue;
        NAXIS1 : CardValue;
	NAXISn : NAXISn_Arr;
        PCOUNT : CardValue;
        GCOUNT : CardValue;
        GROUPS : CardValue;
	ENDCardPos : Natural;
	ENDCardSet : Boolean;
        end record;

InitMandVals : Primary_Mandatory_Card_Values := (InitVal,InitVal,InitVal,InitVal,
                                                 InitNAXISArrVal,
						 InitVal,InitVal,InitVal,
						 0,False);

MandVals : Primary_Mandatory_Card_Values;


type State_Type is (
	UNSPECIFIED,   -- ?? Ada-code default
	INITIALIZED,   -- Reset_State was called
	PRIMARY_NON_STANDARD, -- SIMPLE = F card found: can calculate Header size biut not DU size
	PRIMARY_STANDARD, -- SIMPLE = T card found
	PRIMARY_NO_DATA,  -- NAXIS = 0
	PRIMARY_IMAGE,    -- NAXIS1 > 0
	RANDOM_GROUPS     -- NAXIS1 = 0
	);

State : State_Type := UNSPECIFIED;



------------------------------------------------------------------
package TIO renames Ada.Text_IO;

procedure DBG_Print 
is
begin
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
 TIO.Put(Boolean'Image(MandVals.NAXISn(I).Read) & " NAXIS" & Integer'Image(I)&" ");
 TIO.Put_Line(MandVals.NAXISn(I).Value);
end loop;
TIO.Put(Boolean'Image(MandVals.PCOUNT.Read) & " PCOUNT ");
TIO.Put_Line(MandVals.PCOUNT.Value);
TIO.Put(Boolean'Image(MandVals.GCOUNT.Read) & " GCOUNT ");
TIO.Put_Line(MandVals.GCOUNT.Value);
TIO.Put(Boolean'Image(MandVals.GROUPS.Read) & " GROUPS ");
TIO.Put_Line(MandVals.GROUPS.Value);
TIO.Put(Boolean'Image(MandVals.ENDCardSet) & " END ");
TIO.Put_Line(Positive'Image(MandVals.ENDCardPos));
TIO.Put_Line(State_Type'Image(State));
end DBG_Print;
-- -----------------------------------------------------------








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
		if ((Card(1..8) = "SIMPLE  ") AND 
 		   (Value.To_Boolean(String(Card(11..30))) = False)) then 
			MandVals.SIMPLE.Value := String(Card(11..30));
			MandVals.SIMPLE.Read  := True;
			State := PRIMARY_NON_STANDARD;

		elsif ((Card(1..8) = "SIMPLE  ") AND 
 		       (Value.To_Boolean(String(Card(11..30))) = True)) then 
			MandVals.SIMPLE.Value := String(Card(11..30));
			MandVals.SIMPLE.Read  := True;
			State := PRIMARY_STANDARD;
		else
			-- ERROR: unexpected card, non standard or broken Header
			null;
		end if;

	end In_INITIALIZED;




	procedure In_PRIMARY_STANDARD
		(Pos  : in Positive;
		 Card : in Card_Type) 
	is
	begin
		if (Card(1..8) = "BITPIX  ") then
			MandVals.BITPIX.Value := String(Card(11..30));
			MandVals.BITPIX.Read  := True;
		
		elsif (Card(1..8) = "NAXIS   " AND
		       (Value.To_Integer(String(Card(11..30))) = 0)) then
			MandVals.NAXIS.Value := String(Card(11..30));
			MandVals.NAXIS.Read := True;
			State := PRIMARY_NO_DATA;
	
		elsif ((Card(1..8) = "NAXIS   ") AND 
		       (Value.To_Integer(String(Card(11..30))) /= 0)) then
			MandVals.NAXIS.Value := String(Card(11..30));
			MandVals.NAXIS.Read := True;
	
		elsif ((Card(1..8) = "NAXIS1  ") AND 
		       (Value.To_Integer(String(Card(11..30))) /= 0)) then
			MandVals.NAXISn(1).Value := String(Card(11..30));
			MandVals.NAXISn(1).Read  := True;
			State := PRIMARY_IMAGE;

		elsif ((Card(1..8) = "NAXIS1  ") AND
		       (Value.To_Integer(String(Card(11..30))) = 0)) then
			MandVals.NAXISn(1).Value := String(Card(11..30));
			MandVals.NAXISn(1).Read  := True;
			State := RANDOM_GROUPS;

		else
			-- ERROR: unexpected card, non standard or broken Header
			null;
		end if;
		
	end In_PRIMARY_STANDARD;



	procedure In_PRIMARY_IMAGE
		(Pos  : in Positive;
		 Card : in Card_Type) 
	is
		Idx : Positive;
	begin
		if (Value.Is_Array(Card,"NAXIS",2,NAXIS_Last,Idx)) then
			MandVals.NAXISn(Idx).Value := String(Card(11..30));
			MandVals.NAXISn(Idx).Read  := True;

		else
			-- ERROR: unexpected card, non standard or broken Header
			null;
		end if;

	end In_PRIMARY_IMAGE;



	procedure In_RANDOM_GROUPS
		(Pos  : in Positive;
		 Card : in Card_Type) 
	is
		Idx : Positive;
	begin
		if (Value.Is_Array(Card,"NAXIS",2,NAXIS_Last,Idx)) then
			MandVals.NAXISn(Idx).Value := String(Card(11..30));
			MandVals.NAXISn(Idx).Read  := True;

		elsif (Card(1..8) = "PCOUNT  ") then
			MandVals.PCOUNT.Value := String(Card(11..30));
			MandVals.PCOUNT.Read  := True;

		elsif (Card(1..8) = "GCOUNT  ") then
			MandVals.GCOUNT.Value := String(Card(11..30));
			MandVals.GCOUNT.Read  := True;
		
		elsif (Card(1..8) = "GROUPS  ") then
			MandVals.GROUPS.Value := String(Card(11..30));
			MandVals.GROUPS.Read := True;
		
		else
			-- ERROR: unexpected card, non standard or broken Header
			null;
		end if;

	end In_RANDOM_GROUPS;








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
			
			State := UNSPECIFIED;
		end if;

		case(State) is
			when INITIALIZED =>
			       In_INITIALIZED  (Pos, Card);
			when PRIMARY_NON_STANDARD =>
				null;
			when PRIMARY_STANDARD => 
			       In_PRIMARY_STANDARD(Pos, Card);
			when PRIMARY_NO_DATA =>
				null;
			when PRIMARY_IMAGE => 
			       In_PRIMARY_IMAGE(Pos, Card);	
			when RANDOM_GROUPS => 
			       In_RANDOM_GROUPS(Pos, Card);	
			when others => 
				-- including UNSPECIFIED
				-- programming error: Reset_State 
				-- must be called before New_Card
				null;
		end case;
		
		return Rc;

	end Next;
	



	--
	-- Interface
	--


	function  Next
		(BlockNum  : in Positive; 
		 CardBlock : in Card_Block) return Read_Control
	is
		Rc : Read_Control;
		CardPosBase : Natural := (BlockNum-1) * 36;
		CardPos : Positive;
		Card : Card_Type;
	begin
		for I in CardBlock'Range 
		loop
			Card := CardBlock(I);
			
			if ( Card = ENDCard OR Value.Is_ValuedCard(Card) ) then

				CardPos := CardPosBase + I;
				Rc := Next(CardPos, Card);

				case (Rc) is
					when Stop => 
						exit;
					when StartFromBegining =>
						exit;
					when Continue =>
						null;
				end case;

			end if;

		end loop;
		return Rc;
	end Next;
	






	
	--
	-- Collect Results
	--
	
	-- This is not Lexar, rather grammar rules
	--
	-- it is here that we check whether all values were set and so results are valid
	-- also interpret here semantics, like: 
	--  * if NAXIS exist, also NAXIS1... array must exist
	--  * if one card of a group present, all other cards of that group-type must be present
	--  * recognize alternative calibration sets cccccA card set and cccccB card set
	--  That all data for SizeClaculation was Read/Set
	--  and all those data is valid/within the range
	function  Get return HDU_Size_Info_Type
	is
		Results : HDU_Size_Info_Type;
	begin
		return Results;
	end Get;


end Primary_Size_Info;


