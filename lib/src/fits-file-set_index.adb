
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

	BlockSize_SIOunits : constant SIO.Positive_Count := 2880;
        
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
			Read_Card(File, HeaderStart, CurBlkNum, Blk, CardNum, Card);
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




