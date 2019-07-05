--
-- HDU may be "too big" to keep in memory. This module operates with buffer.
--

with FITS.Header; -- FIXME only temp: Card_Block is also defined in FITS.ads

generic
 Type  Source_Type is limited private;
 with function Next (Source : Source_Type) return FITS.Header.Card_Block;
package FITS.HDU is

	-- establish HDU Type and its dimensions
	
	type HDUPos_Type is (PRIMARY, RANDGROUPS, CONF_EXT);
	
	type HDU_Type ( HDUType : HDUPos_Type;
	                NAXIS   : NAXIS_Type ) is
		record
			CardCount : Positive;
			BITPIX : Positive;
			NAXISn : FITS.Header.NAXIS_Arr(1..NAXIS);
			case HDUType is
				when PRIMARY =>
					null;
				when RANDGROUPS .. CONF_EXT =>
					PCOUNT   : Positive;
					GCOUNT   : Positive;
			end case;
		end record;
	
	
	function Read_Header (Source : Source_Type) return HDU_Type;
	
	procedure Write_Header (Source : Source_Type; HDUType : HDU_Type) is null;




end FITS.HDU;
