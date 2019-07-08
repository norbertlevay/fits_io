--
-- Multi-HDU FITS: Many HDU's are stored in one File. This module describes access (Read/Write) 
-- to one particular HDU.
--
-- HDU may be "too big" to keep in memory. Source/Sink_Type is "container" holding the HDU; usually
-- file on a disk. This module operates on buffer.
--
-- FIXME is this really needed? or use directéy .File ads? (back in old FITS with only one HDU this would not be needed HDU was Media(File) itself, but now with Extensions: e.g. Prim + N-extensions[+ unspec datablocks] in one File, this might reflect multi-HDU files better??)    Then File would have ops like Set_HDU, Copy_HDU, Remove_HDU...
-- TODO add generic param N to configure buffer size (N-blocks)

with FITSlib.Header; use FITSlib.Header;

generic
 Type  Source_Type is limited private;
 Type  Sink_Type   is limited private;
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
	
	procedure Write_Header (Sink : Sink_Type; HDUType : HDU_Type) is null;




end FITSlib.HDU;
