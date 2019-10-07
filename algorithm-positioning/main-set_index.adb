
with Ada.Text_IO; use Ada.Text_IO;

with Formulas;
with Primary_Size_Info;
use Primary_Size_Info;

with Ext_Strict;

separate(main)
procedure Set_Index
           (File   : SIO.File_Type;
            HDUNum : Positive)
is
	CurHDUNum : Positive;
	PrimaryHeaderStart : SIO.Positive_Count;
	ExtHeaderStart : SIO.Positive_Count;
	BlkNum : Positive;
	Blk : Card_Block;
	Rc  : Read_Control;
	Stoped : Boolean;
	HDUSizeInfo : HDU_Size_Info_Type;
	HDUSize_blocks : Formulas.Positive_Count;


	BlockSize_SIOunits : constant SIO.Positive_Count := 2880;
begin

	PrimaryHeaderStart := 1;
	SIO.Set_Index(File,PrimaryHeaderStart);

	CurHDUNum := 1;

	if(CurHDUNum = HDUNum) then
		return;
	end if;

-- Read Primary HDU
	
	Reset_State;

	loop
	 	Card_Block'Read(SIO.Stream(File), Blk);

		BlkNum := Positive( SIO.Index(File) / BlockSize_SIOunits );

		Rc := Next(BlkNum, Blk);

		case(Rc) is
			when Continue =>
				Stoped := False;
			when StartFromBegining =>
				SIO.Set_Index(File,PrimaryHeaderStart);
				Stoped := False;
			when Stop =>
				Stoped := True;
		end case;

		exit when Stoped;

	end loop;

	HDUSizeInfo := Get;
	HDUSize_blocks := Formulas.Calc_HDU_Size_blocks(HDUSizeInfo);

	ExtHeaderStart := PrimaryHeaderStart + SIO.Positive_Count(HDUSize_blocks) * BlockSize_SIOunits;

	SIO.Set_Index(File,ExtHeaderStart);

	-- DEBUG START
	TIO.Put_Line("HDUSize [blocks]: " & Formulas.Positive_Count'Image(HDUSize_blocks));
	
--	Card_Block'Read(SIO.Stream(File), Blk);
--	for I in Blk'Range
--	loop
--		TIO.Put_Line(String(Blk(I)));
--	end loop;
	-- DEBUG END 
	-- FIXME REMOVE DEBUG before implementing Extension


-- Read Extension HDU"s if exist, 
	-- how to handle Unspecified Ext if exist at fits-file end ?

CurHDUNum := CurHDUNum + 1;

while ( CurHDUNum < HDUNum )
loop

	Ext_Strict.Reset_State;

	loop
	 	Card_Block'Read(SIO.Stream(File), Blk);

		--Put_Line("DBG> " & SIO.Positive_Count'Image(SIO.Index(File) - ExtHeaderStart));
--		Put_Line("DBG> " & SIO.Positive_Count'Image(BlockSize_SIOunits));

		BlkNum := Positive((SIO.Index(File) - ExtHeaderStart)/ BlockSize_SIOunits);
		--Put_Line("DBG> " & Positive'Image(BlkNum));
		
		Rc := Ext_Strict.Next(BlkNum, Blk);

		case(Rc) is
			when Continue =>
				null;
			when StartFromBegining =>
				SIO.Set_Index(File, ExtHeaderStart);
			when Stop =>
				exit;
		end case;

	end loop;

	HDUSizeInfo := Ext_Strict.Get;
	HDUSize_blocks := Formulas.Calc_HDU_Size_blocks(HDUSizeInfo);

	ExtHeaderStart := ExtHeaderStart + SIO.Positive_Count(HDUSize_blocks) * BlockSize_SIOunits;

	SIO.Set_Index(File, ExtHeaderStart);

CurHDUNum := CurHDUNum + 1;

end loop;

end Set_Index;




