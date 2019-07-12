--
-- HDU modules provides properties of HDU like type, size, data dimensions...
-- and Read/Write access to Header and Data _within_ the selected HDU.
-- (Use File.Set_HDU to select one particular HDU.)
--
-- NOTE both (File and HDU) operate on File_Type/Source_Type
-- and provide buffered access.
-- 
-- HDU module uses info (type records) specified in Headers to calculate 
-- HDU properties.
--
-- HDU may be "too big" to keep in memory. Source/Sink_Type 
-- is "container" holding the HDU; usually file on a disk. 
--
-- Multi-HDU FITS: many HDU's are stored in one File. 
-- This module describes access (Read/Write) to one particular HDU.
--
-- ALTERNATIVE: don't separate, have only File and
-- disctiguish ops with HDU vs on HDU by function names (prefix HDU_):
-- Remove_HDU(File, HDUNum) vs HDU_Read_Primary_Header(File, PrimHead)
--
-- FIXME _consider_ implement one common DataDimensions_Type and 
-- DataSize calculation routine for all three types below.
-- Use it also for list() of HDU info --> e.g. how to map
-- the three types PrimIMAGE COnfExt and RandGroup record fields
-- to ome common suitable for list() HD-info ?

with FITSlib.Header; use FITSlib.Header;


generic
 type  Buffered_Source_Type is limited private;
 type  Buffered_Sink_Type   is limited private;
 with function Next_Buffer_Content (Source : Buffered_Source_Type) return Card_Block;
package FITSlib.HDU is

	-- --------------------
	-- HDU properties
	-- -------------------- 

	-- FIXME Read_ or Get_ ?? File-Index is moved(?).

	type Conforming_Extension_Form is
                record
                        HDUVar : HDU_Variant;
                        PCOUNT : Natural;
                        GCOUNT : Positive;
                end record;

	procedure Get
		(BSource : in  Buffered_Source_Type;
		 ExtForm : out Conforming_Extension_Form) is null;


	function Read_HDU_Size_bits
		(Source : Buffered_Source_Type) return Natural;


	type Data_Dimensions_Type is
                record
                        HDUVar     : HDU_Variant;
                        CardsCount : Positive;
                        BITPIX     : Integer;
                        NAXIS      : Natural;
                        NAXISn     : NAXIS_Arr(NAXIS_Range);
                end record;

	procedure Read_Data_Dimensions
                (Source : Buffered_Source_Type;
                 DDims  : out Data_Dimensions_Type);
   


	-- experimental with generic Read_Cards
	procedure Read_Exp 
		(File : Buffered_Source_Type; 
		 DSize : out Natural);
	procedure Read_Exp 
		(File : Buffered_Source_Type; 
		 DDims : out Data_Dimensions_Type);
	

end FITSlib.HDU;
