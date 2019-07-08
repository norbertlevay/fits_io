

with Ada.Strings.Fixed; -- Trim

with FITSlib.Header;   use FITSlib.Header;
with FITSlib.Formulas; use FITSlib.Formulas;


package body FITSlib.HDU is




	-- determine Data size of an unknown HDU type
	function Read_Data_Size_bits
		(Source  : Source_Type)
		return Natural
	is
		Nbits : Natural := 0;
		HBlk  : Card_Block;
		HEnd  : HeaderSize_Type;
		What  : What_File;
		Prim  : Standard_Primary_Type;
		PrimImg    : Primary_Image_Type;
		RandGroups : Random_Groups_Type;
		ConfExt    : Conforming_Extension_Type;
	begin

		HBlk := Next(Source);
		-- read first block (after Set_Index(1 or n))
		
		Parse_First_Block(HBlk, What);
		case What is
			when NOT_FITS_FILE => 
				null;
				-- raise exception if Index() is 1 
				-- (if >1 it will be unspecified extension)
				-- FIXME better solution here ? if not a fitsfile 
				-- we should not even call this function

			when NON_STANDARD_PRIMARY => 
				-- raise exception and exit
				null;

			when STANDARD_PRIMARY =>
				Parse(HBlk, Prim);

			when CONF_EXT =>
				Parse(HBlk, ConfExt);
				loop
					HBlk := Next(Source);
					Parse(HBlk, ConfExt);
					Parse(HBlk, HEnd);
					exit when HEnd.ENDCardFound;
				end loop;
				Nbits := ConformingExtension_DataSize_bits
				          (ConfExt.BITPIX,
					   ConfExt.NAXISn(ConfExt.NAXISn'First..ConfExt.NAXIS),
					   ConfExt.PCOUNT,
					   ConfExt.GCOUNT);
				return Nbits;
				-- FIXME not nice return here (due to case Prim below)
		end case;



		case Prim is
			when NO_DATA =>
				-- HDU wihtout DataUnit: HDUSize = HeadSize
				-- FIXME what to do ?? read until end of header?
				-- or leave FileIndex after 1st block?
				Nbits := 0;

			when RANDOM_GROUPS => 
				Parse(HBlk, RandGroups);
				loop
					HBlk := Next(Source);
					Parse(HBlk, RandGroups);
					Parse(HBlk, HEnd);
					exit when HEnd.ENDCardFound;
				end loop;
				Nbits := RandomGroups_DataSize_bits
					(RandGroups.BITPIX,
					 RandGroups.NAXISn(RandGroups.NAXISn'First..RandGroups.NAXIS),
					 RandGroups.PCOUNT,
					 RandGroups.GCOUNT);

			when IMAGE =>
				Parse(HBlk, PrimImg);

				loop
					HBlk := Next(Source);
					Parse(HBlk, PrimImg);
					Parse(HBlk, HEnd);
					exit when HEnd.ENDCardFound;
				end loop;
				Nbits := PrimaryImage_DataSize_bits
					(PrimImg.BITPIX,
					 PrimImg.NAXISn(PrimImg.NAXISn'First..PrimImg.NAXIS));

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
