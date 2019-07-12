

with Ada.Strings.Fixed; -- Trim

with FITSlib.Parser;   use FITSlib.Parser;
with FITSlib.Header;   use FITSlib.Header;
with FITSlib.Formulas; use FITSlib.Formulas;

with Ada.Text_IO;

package body FITSlib.HDU is


        procedure Read_Conforming_Extensions
                (Source     : Buffered_Source_Type;
                 FirstBlock : Card_Block;
		 HEnd       : out HeaderSize_Type;
		 ConfExt    : out Conforming_Extension_Type)
	is
		HBlk    : Card_Block;
	begin
		Parse(FirstBlock, ConfExt);
		loop
			HBlk := Next_Buffer_Content(Source);
			Parse(HBlk, ConfExt);
			Parse(HBlk, HEnd);
			exit when HEnd.ENDCardFound;
		end loop;
	end Read_Conforming_Extensions;



        function Read_Conforming_Extensions_Data_Size_bits
                (Source     : Buffered_Source_Type;
                 FirstBlock : Card_Block) return Natural
	is
		ConfExt : Conforming_Extension_Type;
		First   : Positive;
		HDummy  : HeaderSize_Type;
	begin

		Read_Conforming_Extensions
			(Source, FirstBlock, HDummy, ConfExt);

		First := ConfExt.NAXISn'First;

		return ConformingExtension_DataSize_bits
		          (ConfExt.BITPIX,
			   ConfExt.NAXISn(First..ConfExt.NAXIS),
			   ConfExt.PCOUNT,
			   ConfExt.GCOUNT);
		
	end Read_Conforming_Extensions_Data_Size_bits;








        procedure Read_Random_Groups
		(Source     : Buffered_Source_Type;
		 FirstBlock : Card_Block;
		 HEnd       : out HeaderSize_Type;
		 RandGroups : out Random_Groups_Type)
	is
		HBlk    : Card_Block;
	begin
		Parse(FirstBlock, RandGroups);
		loop
			HBlk := Next_Buffer_Content(Source);
			Parse(HBlk, RandGroups);
			Parse(HBlk, HEnd);
			exit when HEnd.ENDCardFound;
		end loop;
	end Read_Random_Groups;
	

        function Read_Random_Groups_Data_Size_bits 
		(Source     : Buffered_Source_Type;
		 FirstBlock : Card_Block) return Natural
	is
		RandGroups : Random_Groups_Type;
		First      : Positive;
		HDummy  : HeaderSize_Type;
	begin
	        Read_Random_Groups
			(Source, FirstBlock, HDummy, RandGroups);

		First := RandGroups.NAXISn'First;

		return RandomGroups_DataSize_bits
			(RandGroups.BITPIX,
			 RandGroups.NAXISn(First .. RandGroups.NAXIS),
			 RandGroups.PCOUNT,
			 RandGroups.GCOUNT);

	end Read_Random_Groups_Data_Size_bits;

-- experimental: use generic Parser

        procedure Read_Primary_Image 
		(Source     : Buffered_Source_Type;
		 PrimImg    : out Primary_Image_Type)
	is
		function Parse_Cards_For_PrimImg(Pos : Positive; Blk : Card_Arr) 
			return Boolean
		is
			HEnd : HeaderSize_Type;
			--CardPos : Positive;
		begin
			--CardPos := I + (Pos-1)*CardsPerBlock;
			Parse(Blk, PrimImg);
			-- check for END card
                        -- Match_Card(CardPos, Blk(I), HEnd);
			Parse(Blk, HEnd);
			return HEnd.ENDCardFound;
		end Parse_Cards_For_PrimImg;

		procedure Read_Cards_PrimImg is 
			new Read_Cards
		(Source_Type => Buffered_Source_Type,
		 Next        => Next_Buffer_Content,
		 First_Block => First_Block_Null,
		 Parse_Cards => Parse_Cards_For_PrimImg);
	begin
		Read_Cards_PrimImg(Source);
	end Read_Primary_Image;
	

        procedure Read_Primary 
		(Source     : Buffered_Source_Type;
		 FirstBlock : Card_Block;
		 HEnd       : out HeaderSize_Type;
		 PrimImg    : out Primary_Image_Type)
	is
		HBlk    : Card_Block;
	begin
		Parse(FirstBlock, PrimImg);
		loop
			HBlk := Next_Buffer_Content(Source);
			Parse(HBlk, PrimImg);
			Parse(HBlk, HEnd);
			exit when HEnd.ENDCardFound;
		end loop;
	end Read_Primary;
	

        function Read_Primary_Data_Size_bits 
		(Source     : Buffered_Source_Type;
		 FirstBlock : Card_Block) return Natural
	is
		PrimImg : Primary_Image_Type;
		First   : Positive;
		HDummy  : HeaderSize_Type;
	begin
		Read_Primary
			(Source, FirstBlock, HDummy, PrimImg);

		First := PrimImg.NAXISn'First;

		return PrimaryImage_DataSize_bits
			(PrimImg.BITPIX,
			 PrimImg.NAXISn(First .. PrimImg.NAXIS));

	end Read_Primary_Data_Size_bits;







	-- determine Data size of an unknown HDU type
	function Read_Data_Size_bits
		(Source  : Buffered_Source_Type)
		return Natural
	is
		Nbits : Natural := 0;
		-- analyze first block (after caller did Set_Index(1 or n)
		HBlk : Card_Block  := Next_Buffer_Content(Source);
		Var  : HDU_Variant := Parse(HBlk);
	begin

		case Var is
			when UNKNOWN => 
				null;
				-- raise exception if Index() is 1 
				-- (if >1 it will be unspecified extension)
				-- FIXME better solution here ? if not a fitsfile 
				-- we should not even call this function

			when PRIM_UNKNOWN => 
				-- raise exception and exit
				null;

			when PRIM_NON_STANDARD =>
				-- raise exception and exit
				null;

			when PRIM_NO_DATA =>
				-- FIXME what to do ?? read until end of header?
				-- or leave FileIndex after 1st block?
				Nbits := 0;

			when PRIM_IMAGE =>
				Nbits := Read_Primary_Data_Size_bits (Source, HBlk);

			when RAND_GROUPS => 
				Nbits := Read_Random_Groups_Data_Size_bits (Source, HBlk);

			when EXT_IMAGE .. EXT_BINTABLE =>
				Nbits := Read_Conforming_Extensions_Data_Size_bits (Source, HBlk);
			
			when EXT_UNKNOWN =>
				-- raise exception and exit
				null;
		end case;

		return Nbits;

	-- FIXME still missing check if all cards for all fields in structs were found
		-- while Parse() calls.

	end Read_Data_Size_bits;


        procedure Read_Data_Dimensions
                (Source : Buffered_Source_Type;
                 DDims  : out Data_Dimensions_Type)
        is
                HSize      : HeaderSize_Type;
                ConfExt    : Conforming_Extension_Type;
                RandGroups : Random_Groups_Type;
                PrimImg    : Primary_Image_Type;
                -- analyze first block (after caller did Set_Index(1 or n)
                HBlk : Card_Block  := Next_Buffer_Content(Source);
                Var  : HDU_Variant := Parse(HBlk);
        begin
                HSize.CardCount := 0;
                DDims.HDUVar := Var;

                case Var is
                        when UNKNOWN =>
                                null;
                                -- raise exception if Index() is 1 
                                -- (if >1 it will be unspecified extension)
                                -- FIXME better solution here ? if not a fitsfile 
                                -- we should not even call this function

                        when PRIM_UNKNOWN =>
                                -- raise exception and exit
                                null;

                        when PRIM_NON_STANDARD =>
                                -- raise exception and exit
                                null;

                        when PRIM_NO_DATA =>
                                -- FIXME what to do ?? read until end of header?
                                -- or leave FileIndex after 1st block?
                                DDims.NAXIS := 0;

                        when PRIM_IMAGE =>
                                Read_Primary (Source, HBlk, HSize, PrimImg);
                                DDims.CardsCount := HSize.CardCount;
                                DDims.BITPIX := PrimImg.BITPIX;
                                DDims.NAXIS  := PrimImg.NAXIS;
                                DDims.NAXISn := PrimImg.NAXISn;

                        when RAND_GROUPS =>
                                Read_Random_Groups (Source, HBlk, HSize, RandGroups);
                                DDims.CardsCount := HSize.CardCount;
                                DDims.BITPIX := RandGroups.BITPIX;
                                DDims.NAXIS  := RandGroups.NAXIS;
                                DDims.NAXISn(1) := 1;
                                DDims.NAXISn(2 .. RandGroups.NAXISn'Last) := RandGroups.NAXISn;

                        when EXT_IMAGE .. EXT_BINTABLE =>
                                Read_Conforming_Extensions (Source, HBlk, HSize, ConfExt);
                                DDims.CardsCount := HSize.CardCount;
                                DDims.BITPIX := ConfExt.BITPIX;
                                DDims.NAXIS  := ConfExt.NAXIS;
                                DDims.NAXISn := ConfExt.NAXISn;

                        when EXT_UNKNOWN =>
                                -- raise exception and exit
                                null;
                end case;

        end Read_Data_Dimensions;




        -- -----------------------------------------------------------------------------
        -- exprimental, later HDU not generic but this func only
        -- -----------------------------------------------------------------------------


        -- test  : try to create read func for type X
        -- using types from Header; 
        -- first: instantiate generic for it


        Var  : HDU_Variant;
        HEnd : HeaderSize_Type := (False, 0);


        function a_First_Block(Blk : Card_Arr) return Boolean
        is
                Cont : Boolean := True;
                coff : Positive := 1; -- FIXME from Header.Parse func 
        begin
                Var := Parse(Blk);

                case Var is
                        when UNKNOWN .. PRIM_NO_DATA =>
                                Cont := False;
                        when PRIM_IMAGE .. EXT_BINTABLE =>
                                Cont := True;
                        when EXT_UNKNOWN =>
                                Cont := False;
                end case;

                Parse(Blk, HEnd);
                HEnd.CardCount := coff;
                if(HEnd.ENDCardFound) then
                        Cont := False;
                end if;


                return Cont;
        end a_First_Block;


        PrimImg    : Primary_Image_Type;
        RandGroups : Random_Groups_Type;
        ConfExt    : Conforming_Extension_Type;

        function a_Parse_Cards (Pos : Positive; Blk : Card_Arr) return Boolean
        is
                Cont : Boolean := True;
                CardPos : Positive;
        begin

                for I in Blk'Range
                loop
                        case Var is
                                when UNKNOWN .. PRIM_NO_DATA =>
                                        Cont := False;
                                when PRIM_IMAGE =>
                                        ParseCard(Blk(I), PrimImg);
                                        Cont := True;
                                when RAND_GROUPS =>
                                        ParseCard(Blk(I), RandGroups);
                                        Cont := True;
                                when EXT_IMAGE .. EXT_BINTABLE =>
                                        ParseCard(Blk(I), ConfExt);
                                        Cont := True;
                                when EXT_UNKNOWN =>
                                        Cont := False;
                        end case;

                        CardPos := I + (Pos-1)*CardsPerBlock;
                        Match_Card(CardPos, Blk(I), HEnd);
                        if(HEnd.ENDCardFound) then
                                Cont := False;
                        end if;

                end loop;

        return Cont;
        end a_Parse_Cards;


        procedure Read_Cards_Exp
           is new Read_Cards
                (Source_Type => Buffered_Source_Type,
                 Next        => Next_Buffer_Content,
                 First_Block => a_First_Block,
                 Parse_Cards => a_Parse_Cards);





         procedure Read_Exp (File : Buffered_Source_Type; DDims : out Data_Dimensions_Type)
        is
        begin
                Read_Cards_Exp(File);

                -- now Prim filled in, convert to Exp

                DDims.HDUVar     := Var;
                DDims.CardsCount := HEnd.CardCount;
                case Var is
                        when UNKNOWN .. PRIM_NO_DATA =>
                                null;

                        when PRIM_IMAGE =>
                                DDims.BITPIX := PrimImg.BITPIX;
                                DDims.NAXIS  := PrimImg.NAXIS;
                                DDims.NAXISn := PrimImg.NAXISn;

                        when RAND_GROUPS =>
                                DDims.BITPIX     := RandGroups.BITPIX;
                                DDims.NAXIS      := RandGroups.NAXIS;
                                DDims.NAXISn(1)  := 1;-- DDims NAXISn is type Positive
                                DDims.NAXISn(2 .. RandGroups.NAXISn'Last) := RandGroups.NAXISn;

                        when EXT_IMAGE .. EXT_BINTABLE =>
                                DDims.BITPIX     := ConfExt.BITPIX;
                                DDims.NAXIS      := ConfExt.NAXIS;
                                DDims.NAXISn     := ConfExt.NAXISn;

                        when EXT_UNKNOWN =>
                                null;
                end case;

        end Read_Exp;



       procedure Read_Exp (File : Buffered_Source_Type; DSize : out Natural)
       is
              First   : Positive;
        begin
                Read_Cards_Exp(File);

                -- now Prim filled in, convert to Exp

                case Var is
                        when UNKNOWN .. PRIM_NON_STANDARD =>
				null;

                        when PRIM_NO_DATA =>
                                DSize :=0;

                        when PRIM_IMAGE =>
				First := PrimImg.NAXISn'First;
				DSize := PrimaryImage_DataSize_bits
					(PrimImg.BITPIX,
					 PrimImg.NAXISn(First .. PrimImg.NAXIS));

                        when RAND_GROUPS =>
				First := RandGroups.NAXISn'First;
				DSize := RandomGroups_DataSize_bits
					(RandGroups.BITPIX,
					 RandGroups.NAXISn(First .. RandGroups.NAXIS),
					 RandGroups.PCOUNT,
					 RandGroups.GCOUNT);

                        when EXT_IMAGE .. EXT_BINTABLE =>
				First := ConfExt.NAXISn'First;
				DSize := ConformingExtension_DataSize_bits
					(ConfExt.BITPIX,
					 ConfExt.NAXISn(First..ConfExt.NAXIS),
					 ConfExt.PCOUNT,
					 ConfExt.GCOUNT);

                        when EXT_UNKNOWN =>
                                null;
                end case;

        end Read_Exp;

end FITSlib.HDU;
