
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


type State_Type is (
	UNSPECIFIED,   -- code default
	INITIALIZED,   -- Reset_State was called, FA ready to accept cards
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
TIO.Put_Line(State_Type'Image(State));
end DBG_Print;
-- -----------------------------------------------------------








	function Reset_State return Positive
	is
	begin
		MandVals := InitMandVals;
		State := INITIALIZED;
		return 1; -- start FA from 1st card of HDU
	end Reset_State;




	--
	-- State Transition functions
	--

	function In_INITIALIZED
		(Pos  : in Positive;
		Card : in Card_Type) return Natural
	is
		CardVal : Boolean;
	begin
		if (Card(1..8) = "SIMPLE  ") then 
                       
		        CardVal := To_Boolean(Card(11..30));

			MandVals.SIMPLE.Value := Card(11..30);
			MandVals.SIMPLE.Read  := True;
		   
			if(CardVal) 
			then 
				State := PRIMARY_STANDARD;
			else
				State := PRIMARY_NON_STANDARD;
			end if;

		else
			Raise_Exception(Unexpected_Card'Identity, Card);
		end if;

		return Pos + 1;
	end In_INITIALIZED;




	function In_PRIMARY_STANDARD
		(Pos  : in Positive;
		 Card : in Card_Type) return Natural
	is
		CardVal : Integer;
	begin
		if (Card(1..8) = "BITPIX  ") then
			MandVals.BITPIX.Value := String(Card(11..30));
			MandVals.BITPIX.Read  := True;
		
		elsif (Card(1..8) = "NAXIS   ") then

			CardVal := To_Integer(Card(11..30));

			MandVals.NAXIS.Value := Card(11..30);
			MandVals.NAXIS.Read := True;
	
			if (CardVal = 0) then
				State := PRIMARY_NO_DATA;
				MandVals.HDUTypeVal := PRIMARY_WITHOUT_DATA;
				MandVals.HDUTypeSet := True;
			end if;

		elsif (Card(1..8) = "NAXIS1  ") then

			CardVal := To_Integer(Card(11..30));

			MandVals.NAXISn(1).Value := Card(11..30);
			MandVals.NAXISn(1).Read := True;
	
			if (CardVal = 0) then
				State := RANDOM_GROUPS;
				MandVals.HDUTypeVal := RANDOM_GROUPS;
				MandVals.HDUTypeSet := True;
			else
				State := PRIMARY_IMAGE;
				MandVals.HDUTypeVal := PRIMARY_IMAGE;
				MandVals.HDUTypeSet := True;
			end if;

		else
			Raise_Exception(Unexpected_Card'Identity, Card);
		end if;
		
		return Pos + 1;
	end In_PRIMARY_STANDARD;



	function In_PRIMARY_IMAGE
		(Pos  : in Positive;
		 Card : in Card_Type) return Natural
	is
		Idx : Positive;
	begin
		if (Is_Array(Card,"NAXIS",2,NAXIS_Max,Idx))
		then
			MandVals.NAXISn(Idx).Value := Card(11..30);
			MandVals.NAXISn(Idx).Read  := True;
		else
			null; -- FIXME this is final state: called until ENDCard found
		end if;

		return Pos + 1;
	end In_PRIMARY_IMAGE;



	function In_RANDOM_GROUPS
		(Pos  : in Positive;
		 Card : in Card_Type) return Natural
	is
		Idx : Positive;
	begin
		if (Is_Array(Card,"NAXIS",2,NAXIS_Max,Idx)) then
			MandVals.NAXISn(Idx).Value := Card(11..30);
			MandVals.NAXISn(Idx).Read  := True;

		elsif (Card(1..8) = "PCOUNT  ") then
			MandVals.PCOUNT.Value := Card(11..30);
			MandVals.PCOUNT.Read  := True;

		elsif (Card(1..8) = "GCOUNT  ") then
			MandVals.GCOUNT.Value := String(Card(11..30));
			MandVals.GCOUNT.Read  := True;
		
		elsif (Card(1..8) = "GROUPS  ") then
			MandVals.GROUPS.Value := String(Card(11..30));
			MandVals.GROUPS.Read := True;

			-- FIXME see table C.1 : GROUPS value must be set T
		
		else
			Raise_Exception(Unexpected_Card'Identity, Card);
		end if;

		return Pos + 1;
	end In_RANDOM_GROUPS;







	--
	-- FA interface
	--
	function  Next
		(Pos  : in Positive; 
		 Card : in Card_Type) return Natural
	is
		NextCardPos : Natural;
	begin

		if( (NOT(State = UNSPECIFIED)) AND (Card = ENDCard) )
		then
		        MandVals.ENDCardPos := Pos;
		        MandVals.ENDCardSet := True;
			NextCardPos := 0;-- no more cards
			DBG_Print;
			return NextCardPos;
		end if;

		case(State) is
			when INITIALIZED =>
			       NextCardPos := In_INITIALIZED  (Pos, Card);
			when PRIMARY_NON_STANDARD =>
				NextCardPos := 0;-- no more cards
			when PRIMARY_STANDARD => 
			       NextCardPos := In_PRIMARY_STANDARD(Pos, Card);
			when PRIMARY_NO_DATA =>
				NextCardPos := 0;-- no more cards
			when PRIMARY_IMAGE => 
			       NextCardPos := In_PRIMARY_IMAGE(Pos, Card);	
			when RANDOM_GROUPS => 
			       NextCardPos := In_RANDOM_GROUPS(Pos, Card);	
			when UNSPECIFIED =>
			       raise Programming_Error;	
		end case;
		
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


