--
-- File -> HDU separation allows to separate 
-- operations within HDU(Read/Write/Header/Data) vs 
-- operations on HDUs (remove HDU, copy HDU,..).
-- HOWEVER both (File and HDU) operate on File_Type!!
--
-- ALTERNATIVE: don't separate, have only File and
-- disctiguish ops with HDU vs on HDU by function names (prefix HDU_):
-- Remove_HDU(File, HDUNum) vs HDU_Read_Primary_Header(File, PrimHead)
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

	-- Rule: each of the Read_*() funcs needs FileIndex at HeaderStart

generic
 Type  Source_Type is limited private;
 Type  Sink_Type   is limited private;
 with function Next (Source : Source_Type) return Card_Block;
package FITSlib.HDU is




	function Read_Conforming_Extensions_Data_Size_bits 
		(Source     : Source_Type;
                 FirstBlock : Card_Block) return Natural;
	
	function Read_Random_Groups_Data_Size_bits 
		(Source     : Source_Type;
                 FirstBlock : Card_Block) return Natural;
		 
	function Read_Primary_Data_Size_bits
		(Source     : Source_Type;
                 FirstBlock : Card_Block) return Natural;

	-- read header and calculate size of data (for all known HDU types)
	function Read_Data_Size_bits (Source : Source_Type) return Natural;




	-- writing routines for varios data types ...
	
	procedure Write_Primary_Image (Sink : Sink_Type; Primary : Primary_Image_Type) is null;




-- ----------------------------------------------------------------
-- left here for backward comaptibily for test with fits_io-file.adb
-- ----------------------------------------------------------------

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

       function Read_Header
                (Source  : Source_Type)
                return HDU_Type;

end FITSlib.HDU;
