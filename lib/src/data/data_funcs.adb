

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;



package body Data_Funcs is



-- index from begining of Data Unit

function DU_Block_Index
        (Offset_In_Data_Unit : in Positive_Count; 
         Data_Size_bytes     : in Positive_Count) 
    return Positive_Count
is
    Off_bytes : Positive_Count := Offset_In_Data_Unit * Data_Size_bytes;
begin
    return 1 + (Off_bytes - 1) / BlockSize_bytes;
end DU_Block_Index;
-- FIXME why in bytes ??


function Offset_In_Block
        (Offset_In_Data_Unit : in Positive_Count;
         Data_Elems_In_Block : in Positive_Count) 
    return Positive
is
begin
    return 1 + Positive((Offset_In_Data_Unit - 1) mod Data_Elems_In_Block);
end Offset_In_Block;



-- indexing relative to file start


function File_Block_Index(File : File_Type) return Positive_Count
is
    SIO_Index : Positive_Count := Index(File);
begin
    return (1 + (SIO_Index - 1) / BlockSize_sioelems);
end File_Block_Index;



procedure Set_File_Block_Index
        (File        : File_Type; 
         Block_Index : in Positive_Count)
is
    SIO_Index : Positive_Count := 
        1 + (Block_Index - 1) * BlockSize_sioelems;
begin
    Set_Index(File, SIO_Index);
end Set_File_Block_Index;

end Data_Funcs;

