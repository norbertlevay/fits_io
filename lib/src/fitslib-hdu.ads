--
-- HDU may be "too big" to keep in memory. Source_Type is "container" holding the HDU; usually
-- file on a disk. This module operates on buffer.
--
-- TODO add generic param N to configure buffer size (N-blocks)

with FITSlib.Header; use FITSlib.Header;

generic
 Type  Source_Type is limited private;
 with function Next (Source : Source_Type) return Card_Block;
package FITSlib.HDU is

	-- establish HDU Type and its dimensions
	
	type HDU_Category is (PRIMARY, RANDGROUPS, CONF_EXT);
	
	type HDU_Type ( HDUCat : HDU_Category;
	                NAXIS  : NAXIS_Type ) is
		record
			CardCount : Positive;
			BITPIX : Integer;
			NAXISn : NAXIS_Arr(1..NAXIS);
			case HDUCat is
				when PRIMARY =>
					null;
				when RANDGROUPS .. CONF_EXT =>
					PCOUNT   : Positive;
					GCOUNT   : Positive;
			end case;
		end record;
	
	
	function Read_Header (Source : Source_Type) return HDU_Type;
	
	procedure Write_Header (Source : Source_Type; HDUType : HDU_Type) is null;




end FITSlib.HDU;
