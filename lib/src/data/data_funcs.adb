

with Ada.Streams.Stream_IO;



package body Data_Funcs is

BlockSize_bytes : constant Positive := 2880;
BlockSize_sioelems : constant SIO.Positive_Count 
    := SIO.Positive_Count(2880*8 / Ada.Streams.Stream_Element'Size);
-- size counted in SIO file-index elements



-- indexing within Data Unit of a HDU

function DU_Block_Index
        (Offset_In_Data_Unit    : in Positive; 
        Data_Size_bytes     : in Positive) 
    return Positive
is
Off_bytes : Positive := Offset_In_Data_Unit * Data_Size_bytes;
begin
    return 1 + (Off_bytes - 1) / BlockSize_bytes;
end DU_Block_Index;



function Offset_In_Block
        (Offset_In_Data_Unit    : in Positive;
        Data_Elems_In_Block     : in Positive) 
    return Positive
is
begin
    return 1 + (Offset_In_Data_Unit - 1) mod Data_Elems_In_Block;
end Offset_In_Block;



-- indexing relative to file start


function File_Block_Index(File : SIO.File_Type) return Positive
is
use SIO;
SIO_Index : SIO.Positive_Count := SIO.Index(File);
begin
    return Positive(1 + (SIO_Index - 1) / BlockSize_sioelems);
-- FIXME do proper typing instead of expiciti type conversions
end File_Block_Index;



procedure Set_File_Block_Index
        (File : in SIO.File_Type; 
        Block_Index : in Positive)
is
use SIO;
SIO_Index : SIO.Positive_Count := 
    1 + (SIO.Positive_Count(Block_Index) - 1) * BlockSize_sioelems;
begin
    SIO.Set_Index(File, SIO_Index);
end Set_File_Block_Index;

end Data_Funcs;

