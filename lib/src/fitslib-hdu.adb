

with Ada.Strings.Fixed; -- Trim

with FITSlib.Header;   use FITSlib.Header;
with FITSlib.Formulas; use FITSlib.Formulas;

with Ada.Text_IO;

package body FITSlib.HDU is


        procedure Read_Conforming_Extensions
                (Source     : Source_Type;
                 FirstBlock : Card_Block;
		 HEnd       : out HeaderSize_Type;
		 ConfExt    : out Conforming_Extension_Type)
	is
		HBlk    : Card_Block;
	begin
		Parse(FirstBlock, ConfExt);
		loop
			HBlk := Next(Source);
			Parse(HBlk, ConfExt);
			Parse(HBlk, HEnd);
			exit when HEnd.ENDCardFound;
		end loop;
	end Read_Conforming_Extensions;



        function Read_Conforming_Extensions_Data_Size_bits
                (Source     : Source_Type;
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
		(Source     : Source_Type;
		 FirstBlock : Card_Block;
		 HEnd       : out HeaderSize_Type;
		 RandGroups : out Random_Groups_Type)
	is
		HBlk    : Card_Block;
	begin
		Parse(FirstBlock, RandGroups);
		loop
			HBlk := Next(Source);
			Parse(HBlk, RandGroups);
			Parse(HBlk, HEnd);
			exit when HEnd.ENDCardFound;
		end loop;
	end Read_Random_Groups;
	

        function Read_Random_Groups_Data_Size_bits 
		(Source     : Source_Type;
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





        procedure Read_Primary 
		(Source     : Source_Type;
		 FirstBlock : Card_Block;
		 HEnd       : out HeaderSize_Type;
		 PrimImg    : out Primary_Image_Type)
	is
		HBlk    : Card_Block;
	begin
		Parse(FirstBlock, PrimImg);
		loop
			HBlk := Next(Source);
			Parse(HBlk, PrimImg);
			Parse(HBlk, HEnd);
			exit when HEnd.ENDCardFound;
		end loop;
	end Read_Primary;
	

        function Read_Primary_Data_Size_bits 
		(Source     : Source_Type;
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
		(Source  : Source_Type)
		return Natural
	is
		Nbits : Natural := 0;
		-- analyze first block (after caller did Set_Index(1 or n)
		HBlk : Card_Block  := Next(Source);
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






end FITSlib.HDU;
