--
-- HDU spec
--
-- FIXME is this really needed? or use directéy .File ads? 
-- (back in old FITS with only one HDU this would not be needed HDU was 
-- Media(File) itself, but now with Extensions: e.g. 
-- Prim + N-extensions[+ unspec datablocks] in one File, 
-- this might reflect multi-HDU files better??)
-- Then File would have ops like Set_HDU, Copy_HDU, Remove_HDU...
--
-- File -> HDU separation allows to separate 
-- operations _within_ HDU(Read/Write/Header/Data) vs 
-- operations _with_ HDUs (remove HDU, copy HDU,..).
-- HOWEVER both (File and HDU) operate on File_Type !
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
-- FIXME add generic param N to configure buffer size (N-blocks)
-- FIXME _consider_ implement one common DataDimensions_Type and 
-- DataSize calculation routine for all three types below.
-- Use it also for list() of HDU info --> e.g. how to map
-- the three types PrimIMAGE COnfExt and RandGroup record fields
-- to ome common suitable for list() HD-info ?

with FITSlib.Header; use FITSlib.Header;


generic
 type  Source_Type is limited private;
 type  Sink_Type   is limited private;
 with function Next (Source : Source_Type) return Card_Block;
package FITSlib.HDU is

	--FIXME id FirstBlock needed - review for other solution

	procedure Read_Conforming_Extensions
		(Source     : Source_Type;
                 FirstBlock : Card_Block;
		 HEnd       : out HeaderSize_Type;
                 ConfExt    : out Conforming_Extension_Type);

	procedure Read_Random_Groups
		(Source     : Source_Type;
                 FirstBlock : Card_Block;
		 HEnd       : out HeaderSize_Type;
		 RandGroups : out Random_Groups_Type);

	procedure Read_Primary 
		(Source     : Source_Type;
                 FirstBlock : Card_Block;
		 HEnd       : out HeaderSize_Type;
                 PrimImg    : out Primary_Image_Type);



	function Read_Conforming_Extensions_Data_Size_bits 
		(Source     : Source_Type;
                 FirstBlock : Card_Block) return Natural;
	
	function Read_Random_Groups_Data_Size_bits 
		(Source     : Source_Type;
                 FirstBlock : Card_Block) return Natural;
		 
	function Read_Primary_Data_Size_bits
		(Source     : Source_Type;
                 FirstBlock : Card_Block) return Natural;



	-- read header (for all known HDU types)

	function Read_Data_Size_bits (Source : Source_Type) return Natural;



	type Data_Dimensions_Type is
                record
                        HDUVar     : HDU_Variant;
                        CardsCount : Positive;
                        BITPIX     : Integer;
                        NAXIS      : Natural;
                        NAXISn     : NAXIS_Arr(NAXIS_Range);
                end record;

	procedure Read_Data_Dimensions
                (Source : Source_Type;
                 DDims  : out Data_Dimensions_Type);
   


	-- writing routines for varios data types ...
	
	procedure Write_Primary_Image 
		(Sink : Sink_Type; 
		 Primary : Primary_Image_Type) is null;


	-- -------------------------------------------------------------------------
	-- experimental with generic Read_Cards
	-- -------------------------------------------------------------------------
	
        procedure Read_Exp 
		(File : Source_Type; 
		 DDims : out Data_Dimensions_Type);
	
	procedure Read_Exp 
		(File : Source_Type; 
		 DSize : out Natural);
 


end FITSlib.HDU;
