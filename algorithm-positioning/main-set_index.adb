
with Ada.Text_IO; use Ada.Text_IO;

with FITS; use FITS;
with Formulas;
with Keyword_Record;    use Keyword_Record;
with FA_Primary;
with FA_Extension;

separate(main)
procedure Set_Index
           (File   : SIO.File_Type;
	    Options : String;
            HDUNum : Positive)
is
type Card_Block is array(1..36) of Card_Type;


type Read_Control is
        (Continue,           -- continue calling Next() and supplying CardBlocks
         StartFromBegining,  -- read again CardBlock from begining of Header
         Stop);              -- do not provide more CardBlocks, usually after END-card found
-- this enables implement various parsing strategies including 2-pass parsing (StartFromBegining)

	function To_Prim_Options_Type(Opt : String) return FA_Primary.Options_Type
	is
	begin
		if(Options = "MAND")
		then
			return (True,False);

		elsif(Options = "RES")
		then
			return (True,True);
		else
			return (False,False);
		end if;

	end To_Prim_Options_Type;
	
	function To_Ext_Options_Type(Opt : String) return FA_Extension.Options_Type
	is
	begin
		if(Options = "MAND")
		then
			return (True,False,False,False);

		elsif(Options = "RES")
		then
			return (True,True,True,True);

		else
			return (False,False,False,False);
		end if;

	end To_Ext_Options_Type;

        --
        -- read by blocks
        --
       function  Next
                (HDUNum : in Positive;
		 BlockNum  : in Positive;
                 CardBlock : in Card_Block) return Read_Control
        is
                NextCardPos : Natural;
                Rc : Read_Control := Continue;
                CardPosBase : Natural := (BlockNum-1) * 36;
                CardPos : Positive;
                Card : Card_Type;
        begin
                for I in CardBlock'Range
                loop
                        Card := CardBlock(I);

                        CardPos := CardPosBase + I;

			if(HDUNum = 1)
			then
				NextCardPos := FA_Primary.Next(CardPos, Card);
			else
				NextCardPos := FA_Extension.Next(CardPos, Card);
			end if;
			-- FIXME use generic instead HDUNum

                        -- currently ignored - we loop throu anyway
                        if(NextCardPos = 0)
			then
				Rc := Stop;
				exit;
			else
				Rc := Continue;
			end if;

                end loop;
                return Rc;
        end Next;


	CurHDUNum : Positive;
	PrimaryHeaderStart : SIO.Positive_Count;
	ExtHeaderStart : SIO.Positive_Count;
	BlkNum : Positive;
	Blk : Card_Block;
	Rc  : Read_Control;
	Stoped : Boolean;
--	HDUSizeInfo : HDU_Size_Rec;
	HDUSize_blocks : Formulas.Positive_Count;


	BlockSize_SIOunits : constant SIO.Positive_Count := 2880;
	CardPos : Positive;

	FA_Prim_Options : constant FA_Primary.Options_Type   := To_Prim_Options_Type(Options);	
	FA_Ext_Options  : constant FA_Extension.Options_Type := To_Ext_Options_Type(Options);	
begin

	PrimaryHeaderStart := 1;
	SIO.Set_Index(File,PrimaryHeaderStart);

	CurHDUNum := 1;

	if(CurHDUNum = HDUNum) then
		return;
	end if;

-- Read Primary HDU

	FA_Primary.Configure(FA_Prim_Options);
	-- parse for size-related cards (a.k.a. mandatory cards)

	CardPos := FA_Primary.Reset_State;

	loop
	 	Card_Block'Read(SIO.Stream(File), Blk);

		BlkNum := Positive( SIO.Index(File) / BlockSize_SIOunits );

		Rc := Next(CurHDUNum,BlkNum, Blk);

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
	
	declare
		HDUSizeInfo : HDU_Size_Rec := FA_Primary.Get;
	begin
		HDUSize_blocks := Formulas.Calc_HDU_Size_blocks(HDUSizeInfo);
	end;

	ExtHeaderStart := PrimaryHeaderStart + SIO.Positive_Count(HDUSize_blocks) * BlockSize_SIOunits;

	SIO.Set_Index(File,ExtHeaderStart);

	TIO.Put_Line("DBG> HDUSize [blocks]: " & Formulas.Positive_Count'Image(HDUSize_blocks));
	

-- Read Extension HDU's if exist, 

CurHDUNum := CurHDUNum + 1;

while ( CurHDUNum < HDUNum )
loop

	FA_Extension.Configure(FA_Ext_Options);
	-- parse for size-related cards (a.k.a. mandatory cards)

	CardPos := FA_Extension.Reset_State;

	loop
	 	Card_Block'Read(SIO.Stream(File), Blk);

		BlkNum := Positive((SIO.Index(File) - ExtHeaderStart)/ BlockSize_SIOunits);
		
		Rc := Next(CurHDUNum, BlkNum, Blk);

		case(Rc) is
			when Continue =>
				null;
			when StartFromBegining =>
				SIO.Set_Index(File, ExtHeaderStart);
			when Stop =>
				exit;
		end case;

	end loop;

	declare
		HDUSizeInfo : HDU_Size_Rec := FA_Extension.Get;
	begin
		--HDUSizeInfo    := FA_Extension.Get;
		HDUSize_blocks := Formulas.Calc_HDU_Size_blocks(HDUSizeInfo);
	end;

	ExtHeaderStart := ExtHeaderStart + SIO.Positive_Count(HDUSize_blocks) * BlockSize_SIOunits;

	Put_Line("DBG> New ExtHeaderStart: " & SIO.Positive_Count'Image(ExtHeaderStart));

	SIO.Set_Index(File, ExtHeaderStart);

CurHDUNum := CurHDUNum + 1;

end loop;

-- FIXME add handle Random Blocks if exist at fits-file end 

end Set_Index;




