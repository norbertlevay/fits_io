

with Ada.Strings.Fixed; -- Trim

with FITSlib.Header; use FITSlib.Header;


package body FITSlib.HDU is

	function Read_Primary_Image
		(Source  : Source_Type)
		return Primary_Image_Type
	is
		HBlk  : Card_Block;
		DSize : Primary_Image_Type;
		HEnd  : HeaderSize_Type;
		What  : What_File;
		Prim  : Standard_Primary_Type;
	begin

		HBlk := Next(Source);
		-- read first block (after Set_Index(1))
		
		Parse_First_Block(HBlk, What);
		case What is
			when NOT_FITS_FILE => 
				null;
			when NON_STANDARD_PRIMARY => 
				-- raise exception and exit
				null;
			when STANDARD_PRIMARY =>
				Parse(HBlk, Prim);
		end case;

		case Prim is
			when NO_DATA =>
				-- HDU wihtout DataUnit: HDUSize = HeadSize
				-- FIXME what to do ?? read until end of header?
				null;
			when RANDOM_GROUPS => 
				null; -- we intend to read Primary IMAGE ???? raise exception
			when IMAGE =>
				Parse(HBlk, DSize);
				-- and continue loop reading until Header END
		end case;


		loop
			HBlk := Next(Source);
			Parse(HBlk, DSize);
			Parse(HBlk, HEnd);
			exit when HEnd.ENDCardFound;
		end loop;

		return DSize;
	
	end Read_Primary_Image;





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
