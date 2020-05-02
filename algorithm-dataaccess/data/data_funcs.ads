
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

package Data_Funcs is

BlockSize_bytes    : constant Positive_Count :=  2880;
BlockSize_bits     : constant Positive_Count := 23040;
BlockSize_sioelems : constant Positive_Count
    := BlockSize_bits / Ada.Streams.Stream_Element'Size;
-- size counted in SIO file-index elements

subtype Block_Index_Count is Positive_Count range 1 .. Positive_Count'Last / BlockSize_sioelems;
-- FIXME not in use yet ; MAkes sense ?



-- convert between
-- data-element-index <-> [Block_Index, Offset_Within_Block]



-- Offset to data-element in HDU relative to DU start
-- Offset : 1 .. NAXIS1*NAXIS2* ... *NAXISn
function DU_Block_Index
                (Offset_In_Data_Unit    : in Positive_Count; 
                 Data_Size_bytes        : in Positive_Count) 
        return Positive_Count;

function Offset_In_Block
                (Offset_In_Data_Unit    : in Positive_Count;
                 Data_Elems_In_Block    : in Positive_Count) 
        return Positive;


-- convert between Stream_IO File_Index <-> FITS Block_Index

function File_Block_Index(File : in File_Type) return Positive_Count;
-- index of FITS-block relative to File start

procedure Set_File_Block_Index
        (File        : in File_Type; 
         Block_Index : in Positive_Count);
-- moves SIO.File_Index to first element of the block given by Block_Index

end Data_Funcs;

