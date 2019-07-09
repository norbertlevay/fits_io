

with Ada.Strings.Fixed; -- Trim

with FITSlib.Header;   use FITSlib.Header;
with FITSlib.Formulas; use FITSlib.Formulas;

with Ada.Text_IO;

package body FITSlib.HDU is



        function Read_Conforming_Extensions_Data_Size_bits
                (Source     : Source_Type;
                 FirstBlock : Card_Block) return Natural
	is
		ConfExt : Conforming_Extension_Type;
		HBlk    : Card_Block;
		First   : Positive;
		HEnd    : HeaderSize_Type;
	begin
		Parse(FirstBlock, ConfExt);
		loop
			HBlk := Next(Source);
			Parse(HBlk, ConfExt);
			Parse(HBlk, HEnd);
			exit when HEnd.ENDCardFound;
		end loop;
		First := ConfExt.NAXISn'First;
		return ConformingExtension_DataSize_bits
		          (ConfExt.BITPIX,
			   ConfExt.NAXISn(First..ConfExt.NAXIS),
			   ConfExt.PCOUNT,
			   ConfExt.GCOUNT);
	end Read_Conforming_Extensions_Data_Size_bits;



        function Read_Random_Groups_Data_Size_bits 
		(Source     : Source_Type;
		 FirstBlock : Card_Block) return Natural
	is
		RandGroups : Random_Groups_Type;
		HBlk    : Card_Block;
		First   : Positive;
		HEnd    : HeaderSize_Type;
	begin
		Parse(FirstBlock, RandGroups);
		loop
			HBlk := Next(Source);
			Parse(HBlk, RandGroups);
			Parse(HBlk, HEnd);
			exit when HEnd.ENDCardFound;
		end loop;
		First := RandGroups.NAXISn'First;
		return RandomGroups_DataSize_bits
			(RandGroups.BITPIX,
			 RandGroups.NAXISn(First .. RandGroups.NAXIS),
			 RandGroups.PCOUNT,
			 RandGroups.GCOUNT);
	end Read_Random_Groups_Data_Size_bits;




        function Read_Primary_Data_Size_bits 
		(Source     : Source_Type;
		 FirstBlock : Card_Block) return Natural
	is
		PrimImg : Primary_Image_Type;
		HBlk    : Card_Block;
		First   : Positive;
		HEnd    : HeaderSize_Type;
	begin
		Parse(FirstBlock, PrimImg);
		loop
			HBlk := Next(Source);
			Parse(HBlk, PrimImg);
			Parse(HBlk, HEnd);
			exit when HEnd.ENDCardFound;
		end loop;
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






	-- -------------------------------------------------
	-- backup to save typing
	-- -------------------------------------------------
	--
        -- establish HDU Type and its dimensions


	function Read_Header
		(Source  : Source_Type)
		return HDU_Type
	is
		HBlk  : Card_Block;
		HSize : HeaderSize_Type;
		DSize : DataSize_Type;
		HDUCat : HDU_Category;
	begin
	
		loop
			HBlk := Next(Source);
			Parse(HBlk, DSize);
			Parse(HBlk, HSize);
			exit when HSize.ENDCardFound;
		end loop;


		-- FIXME DSize keys are  Mandatory, check that all cards were read
		-- otherwise depending on missing card might raise exception
		
		
		-- convert Dsize & HSize into HDU_Type


		-- FIXME this procedure should not have parsing of keys; That should be in Header,
		-- and here only call it.
		-- Result: HDU_Type (Prim, RandGroup, IMAGE, TABOE,BINTABLE) + NAXIS or TFIELDS, (PCOUNT,GCOUNT?):
		-- NAXIS: Prim, RandGroups, EXT_IMAGE
		-- TFIELDS: EXT_TABLE, EXT_BINTABLE -> these have NAXIS = 2, BITPIX=8
		-- PCOUNT & GCOUNT only RandGRoups
		-- PCOUNT in BIN_TABLE
		-- Primary: not present, in others: PCOUNT=0 GCOUNT=1
		declare
			SIMPLE   : String := Ada.Strings.Fixed.Trim(DSize.SIMPLE,  Ada.Strings.Both);
			XTENSION : String := Ada.Strings.Fixed.Trim(DSize.XTENSION,Ada.Strings.Both);
		begin
			if (SIMPLE = "" AND XTENSION = "") then
				-- raise exception standard violation
				null;
			end if;
			
			if (SIMPLE = "T") then
				
				if (DSize.NAXISn((1)) = 0) then
					HDUCat := RANDGROUPS;
					-- FIXME how about GROUPS key ?
					else
						HDUCat := PRIMARY;
				end if;
			
			elsif (SIMPLE = "F") then
				-- primary non conformant
				-- don't know what do, raise exception: non-conformant HDU
				null;
			else
				-- standard violation, raise exception
				null;
			end if;
		
			if (XTENSION = "IMAGE") then
				HDUCat := CONF_EXT;
			elsif(XTENSION = "TABLE") then
				HDUCat := CONF_EXT;
			elsif(XTENSION = "BINTABLE") then
				HDUCat := CONF_EXT;
			else
				-- unknown extesion, raise exception unknown
				null;
			end if;
		end;


		-- one of known HDU's found, fill-in and return HDU_Type

		
		declare
			--HDUCat  : HDU_Category := HDUCat;
			Last    : Positive := DSize.NAXIS; 
			-- FIXME should crosscheck NAXIS and max(NAXIS_Arr) whether consistent
			HDUType : HDU_Type(HDUCat, Last);
		begin

			HDUType.CardCount := HSize.CardCount;

			HDUType.BITPIX := DSize.BITPIX;
			HDUType.NAXISn := DSize.NAXISn(1..Last);
			
			case HDUType.HDUCat is
                                when PRIMARY =>
                                        null;
                                when RANDGROUPS .. CONF_EXT =>
                                        HDUType.PCOUNT := DSize.PCOUNT;
                                        HDUType.GCOUNT := DSize.GCOUNT;
                        end case;

			return HDUType;
		end;

	
	end Read_Header;




end FITSlib.HDU;
