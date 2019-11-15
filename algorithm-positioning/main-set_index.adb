-- NOTES:
-- FA_Prim/Ext had Options for configurability (not implemented):
       -- FA configuration
--     type Options_Type is
--              (
--                 ALGORITHM_STRICT,  -- parsing Headers follows strictly FITS-Standard
--                 ALGORITHM_TOLERANT -- parsing Headers fails only if:
                        -- * essential key is missing
                        -- * essential key is duplicate with different values (ambiguity)
--         );
--       procedure Configure(Options : Options_Type) is null;
-- END NOTES


with Ada.Text_IO; use Ada.Text_IO;

with Formulas;
with Keyword_Record;    use Keyword_Record;
with FA_Primary;
with FA_Extension;
with Strict;

separate(main)
procedure Set_Index
           (File   : SIO.File_Type;
            HDUNum : Positive)
is

	type Card_Block is array(1..36) of Card_Type;

	BlockSize_SIOunits : constant SIO.Positive_Count := 2880;
        

	-- buffered read card
	function NextCard (HStart : in SIO.Positive_Count;
			CurBlkNum : in out Natural;
			Blk : in out Card_Block;
			CardNum : in Positive) return Natural
	is
		BlkNum : Positive;
		CardNumInBlk : Positive;
		Card : String(1..80);
		BlkNumIndex : SIO.Positive_Count;
	begin
		BlkNum := 1 + (CardNum - 1) / 36;
		CardNumInBlk := CardNum - (BlkNum - 1) * 36;
	
		if(BlkNum /= CurBlkNUm)
		then
			BlkNumIndex := SIO.Positive_Count( Positive(HStart) + (BlkNum-1) 
						* Positive(BlockSize_SIOunits) );

			SIO.Set_Index(File, BlkNumIndex);
		 	Card_Block'Read(SIO.Stream(File), Blk);
			CurBlkNum := BlkNum;
		end if;

		Card := Blk(CardNumInBlk);

		return Strict.Next(CardNum, Card);
	end NextCard;


	CurHDUNum : Positive;
	PrimaryHeaderStart : SIO.Positive_Count;
	ExtHeaderStart : SIO.Positive_Count;
	Blk : Card_Block;
	HDUSize_blocks : Formulas.Positive_Count;
	CardNum : Natural;
	CurBlkNum : Natural := 0; -- none read yet
begin

	PrimaryHeaderStart := 1;
	SIO.Set_Index(File,PrimaryHeaderStart);

	CurHDUNum := 1;

	if(CurHDUNum = HDUNum) then
		return;
	end if;

	-- Read Primary Header

	CardNum := Strict.Reset_State;
	loop
		CardNum := NextCard(PrimaryHeaderStart, CurBlkNum, Blk, CardNum );
		exit when (CardNum = 0);
	end loop;

	-- calc HDU size
	
	declare
		PSize : Strict.Result_Rec := Strict.Get;
	begin
		HDUSize_blocks := Formulas.Calc_HDU_Size_blocks(PSize.CardsCount, 
							PSize.BITPIX, 
							PSize.NAXISArr);
		TIO.New_Line;Put_Line("DBG> HDU_Type: " & Strict.HDU_Type'Image(PSize.HDU));
	end;

	TIO.Put_Line("DBG> HDUSize [blocks]: " & Formulas.Positive_Count'Image(HDUSize_blocks));

	-- move to next HDU
	
	ExtHeaderStart := PrimaryHeaderStart 
				+ SIO.Positive_Count(HDUSize_blocks) * BlockSize_SIOunits;
	TIO.New_Line;
	Put_Line("DBG> Next ExtHeaderStart: " & SIO.Positive_Count'Image(ExtHeaderStart));
	SIO.Set_Index(File,ExtHeaderStart);
	



-- Read Extension HDU's if exist, 

CurHDUNum := CurHDUNum + 1;

while ( CurHDUNum < HDUNum )
loop

	TIO.New_Line;
	
	-- Read Extension Header
	
	CardNum := Strict.Reset_State;
	loop
		CardNum := NextCard(ExtHeaderStart, CurBlkNum, Blk, CardNum );
		exit when (CardNum = 0);
	end loop;

	-- calc HDU size

	declare
		PSize : Strict.Result_Rec := Strict.Get;
	begin
		HDUSize_blocks := Formulas.Calc_HDU_Size_blocks(PSize.CardsCount, 
							PSize.BITPIX, 
							PSize.NAXISArr);
		TIO.New_Line;Put_Line("DBG> HDU_Type: " & Strict.HDU_Type'Image(PSize.HDU));
	end;

	TIO.Put_Line("DBG> HDUSize [blocks]: " & Formulas.Positive_Count'Image(HDUSize_blocks));
	
	-- move to next HDU

	ExtHeaderStart := ExtHeaderStart 
				+ SIO.Positive_Count(HDUSize_blocks) * BlockSize_SIOunits;
	TIO.New_Line;
	Put_Line("DBG> Next ExtHeaderStart: " & SIO.Positive_Count'Image(ExtHeaderStart));
	SIO.Set_Index(File, ExtHeaderStart);

CurHDUNum := CurHDUNum + 1;

end loop;

-- FIXME add handle Random Blocks if exist at fits-file end 

end Set_Index;




