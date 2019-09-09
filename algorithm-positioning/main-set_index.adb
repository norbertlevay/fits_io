

with Formulas;
with Primary_Size_Info;
use Primary_Size_Info;

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

-- Read Extension HDU"s if exist, 
	-- how to handle Unspecified Ext if exist at fits-file end ?

CurHDUNum := CurHDUNum + 1;

while ( CurHDUNum < HDUNum )
loop

--	Header.Ext_Reset_State;

	loop
--	 	Card_Block'Read(SIO.Stream(File), Blk);

--		BlkNum := To_Block_Count(ExtHeaderStart - SIO.Index(File));
		
		Rc := Stop;--Header.Ext_Next(BlkNum, HBlk);

		case(Rc) is
			when Continue =>
				null;
			when StartFromBegining =>
				SIO.Set_Index(File, ExtHeaderStart);
			when Stop =>
				exit;
		end case;

	end loop;

	HDUSizeInfo := Get;
	HDUSize_blocks := Formulas.Calc_HDU_Size_blocks(HDUSizeInfo);

	ExtHeaderStart := ExtHeaderStart + SIO.Positive_Count(HDUSize_blocks) * BlockSize_SIOunits;

	SIO.Set_Index(File, ExtHeaderStart);

CurHDUNum := CurHDUNum + 1;

end loop;

end Set_Index;




