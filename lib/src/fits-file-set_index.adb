
with Ada.Text_IO; use Ada.Text_IO;

with Formulas;
with Keyword_Record;    use Keyword_Record;
with Strict;

separate(FITS.File)
procedure Set_Index
           (File   : SIO.File_Type;
            HDUNum : Positive)
is

	package TIO renames Ada.Text_IO;

	type Card_Block is array(1..36) of Card_Type;

	BlockSize_SIOunits : constant SIO.Positive_Count := 2880;
        

	-- buffered read card
	procedure Read_Card (HStart : in SIO.Positive_Count;
			CurBlkNum : in out Natural;
			Blk : in out Card_Block;
			CardNum : in Positive;
			Card    : out  String)
	is
		BlkNum : Positive;
		CardNumInBlk : Positive;
		BlkNumIndex : SIO.Positive_Count;
	begin
		BlkNum := 1 + (CardNum - 1) / 36;
		CardNumInBlk := CardNum - (BlkNum - 1) * 36;
	
		if(BlkNum /= CurBlkNUm)
		then
			-- FIXME BEGIN only this section depends on SIO. file access
			-- make it Read_Block(SIO.File, FileBlkNum, Blk)
			-- where FileBlkNum := HStart + BlkNum
			-- BlkNum - relative to HDU start
			-- FileBlkNum - relative to File start
			BlkNumIndex := SIO.Positive_Count( Positive(HStart) + (BlkNum-1) 
						* Positive(BlockSize_SIOunits) );

			SIO.Set_Index(File, BlkNumIndex);
		 	Card_Block'Read(SIO.Stream(File), Blk);
			CurBlkNum := BlkNum;
			-- FIXME END   only this section depends on SIO. file access
		end if;

		Card := Blk(CardNumInBlk);

	end Read_Card;

	function Read_Mandatory (HeaderStart : SIO.Positive_Count) return HDU_Info_Type
	is
		CardNum : Natural;
		Card : String(1..80);

		CurBlkNum : Natural := 0; -- none read yet
		Blk : Card_Block;
	begin
		CardNum := Strict.Reset_State;
		loop
			Read_Card(HeaderStart, CurBlkNum, Blk, CardNum, Card);
			CardNum := Strict.Next(CardNum, Card);
			exit when (CardNum = 0);
		end loop;

		-- calc HDU size

		declare
			PSize   : Strict.Result_Rec := Strict.Get;
			HDUInfo : HDU_Info_Type(PSize.NAXIS_Last);
		begin
			-- convert rec
			
			HDUInfo.XTENSION := Max20.To_Bounded_String("huhu");--Max20.Bounded_String
			HDUInfo.CardsCnt := 1; --FPositive
			HDUInfo.BITPIX := PSize.BITPIX;--Integer; 

-- FIXME unify types:	HDUInfo.NAXISn := PSize.NAXISArr;
			for I in HDUInfo.NAXISn'Range
			loop
				HDUInfo.NAXISn(I) := FInteger(PSize.NAXISArr(I));
			end loop;

			return HDUInfo;
		end;

	end Read_Mandatory;


	Card : String(1..80);
	CurHDUNum : Positive;
	HeaderStart : SIO.Positive_Count;
	Blk : Card_Block;
	HDUSize_blocks : Formulas.Positive_Count;
	CardNum : Natural;
	CurBlkNum : Natural := 0; -- none read yet
begin
	HeaderStart := 1;
	SIO.Set_Index(File,HeaderStart);

	CurHDUNum := 1;

	if(CurHDUNum = HDUNum) then
		return;
	end if;

	while ( CurHDUNum < HDUNum )
	loop
		TIO.New_Line;
	
		-- Read Header
	
		CardNum := Strict.Reset_State;
		loop
			Read_Card(HeaderStart, CurBlkNum, Blk, CardNum, Card);
			CardNum := Strict.Next(CardNum, Card);
			exit when (CardNum = 0);
		end loop;

		-- calc HDU size

		declare
			PSize : Strict.Result_Rec := Strict.Get;
		begin
			HDUSize_blocks := Formulas.Calc_HDU_Size_blocks(PSize.CardsCount, 
							PSize.BITPIX, 
							PSize.NAXISArr);
			TIO.New_Line;Put_Line("DBG> HDU_Type: " 
						& Strict.HDU_Type'Image(PSize.HDU));
		end;

		TIO.Put_Line("DBG> HDUSize [blocks]: " 
					& Formulas.Positive_Count'Image(HDUSize_blocks));
	
		-- move to next HDU

		HeaderStart := HeaderStart 
				+ SIO.Positive_Count(HDUSize_blocks) * BlockSize_SIOunits;
		TIO.New_Line;
		Put_Line("DBG> Next ExtHeaderStart: " & SIO.Positive_Count'Image(HeaderStart));
		
		SIO.Set_Index(File, HeaderStart);

		CurHDUNum := CurHDUNum + 1;

	end loop;

-- FIXME add handle Random Blocks if exist at fits-file end 

end Set_Index;




