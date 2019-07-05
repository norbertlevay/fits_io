

with Ada.Strings.Fixed; -- Trim

with FITS.Header; use FITS.Header;



package body FITS.HDU is


	function Read_Header
		(Source  : Source_Type)
		return HDU_Type
	is
		HBlk  : FITS.Header.Card_Block;
		HSize : HeaderSize_Type;
		DSize : DataSize_Type;
		HDUType : HDU_Category;
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
					HDUType := RANDGROUPS;
					-- FIXME how about GROUPS key ?
					else
						HDUType := PRIMARY;
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
				HDUType := CONF_EXT;
			elsif(XTENSION = "TABLE") then
				HDUType := CONF_EXT;
			elsif(XTENSION = "BINTABLE") then
				HDUType := CONF_EXT;
			else
				-- unknown extesion, raise exception unknown
				null;
			end if;
		end;


		-- one of known HDU's found, fill-in and return HDU_Type

		
		declare
			HDUCat  : HDU_Category := HDUType;
			Last    : Positive := DSize.NAXIS; 
			-- FIXME should crosscheck NAXIS and max(NAXIS_Arr) whether consistent
			HDUType : HDU_Type(HDUCat, Last);
		begin
			
			HDUType.CardCount := HSize.CardCount;

			HDUType.BITPIX := Dsize.BITPIX;
			HDUType.NAXISn := DSize.NAXISn(1..Last);
			
			case HDUType.HDUType is
                                when PRIMARY =>
                                        null;
                                when RANDGROUPS .. CONF_EXT =>
                                        HDUType.PCOUNT := DSize.PCOUNT;
                                        HDUType.GCOUNT := DSize.GCOUNT;
                        end case;

			return HDUType;
		end;

	
	end Read_Header;




end FITS.HDU;
