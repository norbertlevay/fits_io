

with Primary_Size_Info;
with Formulas;


procedure Set_Index
           (File   : SIO.File_Type;
            HDUNum : Positive)
is
	CurHDUNum : Positive;
	PrimaryHeaderStart : SIO.Positive_Count;
	BlkNum : SIO.Poritive_Count;
	Blk : Card_Block;
	Rc  : Header.Read_Control;
	Stoped : Boolean;

begin

	PrimaryHeaderStart := 1;
	SIO.Set_Index(File,PrimaryHeaderStart);

	CurHDUNum := 1;

	if(CurHDUNum = HDUNum)
		return;
	end if;

-- Read Primary HDU
	
	Header.Reset_State;

	loop
	 	Card_Block'Read(SIO.Stream(File), Blk);

		BlkNum := To_Block_Count(SIO.Index(File));

		Rc := Header.Next(BlkNum, HBlk);

		case(Rc)
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

	HDUSize_blocks := To_Blocks(Header.HDU_Size_bits);

	ExtHeaderStart := PrimaryHeaderStart + HDUSize_blocks * BlockSize_SIOunits;

	SIO.Set_Index(File,ExtHeaderStart);

-- Read Extension HDU"s if exist, 
	-- how to handle Unspecified Ext if exist at fits-file end ?

CurHDUNum := CurHDUNum + 1;

while ( CurHDUNum < HDUNum )
loop

	Header.Ext_Reset_State;

	loop
	 	Card_Block'Read(SIO.Stream(File), Blk);

		BlkNum := To_Block_Count(ExtHeaderStart - SIO.Index(File));
		
		Rc := Header.Ext_Next(BlkNum, HBlk);

		case(Rc)
			when Continue =>
				null;
			when StartFromBegining =>
				SIO.Set_Index(File, ExtHeaderStart);
			when Stop =>
				exit;
		end case;

	end loop;

	HDUSize_blocks := To_Blocks(Header.HDU_Size_bits);

	ExtHeaderStart := ExtHeaderStart + HDUSize_blocks * BlockSize_SIOunits;

	SIO.Set_Index(File, ExtHeaderStart);

CurHDUNum := CurHDUNum + 1;

end loop;

end Set_Index;
