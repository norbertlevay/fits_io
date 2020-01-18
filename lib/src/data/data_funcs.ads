
with Ada.Streams.Stream_IO;

package Data_Funcs is

package SIO renames Ada.Streams.Stream_IO;

-- Offset to data-element in HDU relative to DU start
-- Offset : 1 .. NAXIS1*NAXIS2* ... *NAXISn
function DU_Block_Index
                (Offset_In_Data_Unit    : in Positive; 
                Data_Size_bytes         : in Positive) 
        return Positive;
function Offset_In_Block
                (Offset_In_Data_Unit    : in Positive;
                Data_Size_bytes         : in Positive) 
        return Positive;


-- index of Block relative to File start
function File_Block_Index(File : SIO.File_Type) return Positive;
procedure Set_File_Block_Index
		(File : in SIO.File_Type; 
		Block_Index : in Positive);
-- Set func moves SIO file Index to first element 
-- of the block given ba Block_Index

end Data_Funcs;

