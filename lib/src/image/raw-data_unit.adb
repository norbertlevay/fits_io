
-- Sequential Access:
-- if T_Arr'Length = NAXISi*NAXISi-1*...*NAXIS1
-- then repeating Read (NAXISn*NAXISn-1*...NAXISi+1)-times 
-- reads all DU sequentially
-- NOTE position to DUStart before 1st call in Read/Write_x_Data

-- FIXME Read/Write_Array should be 'separate'-file and have two variants:
-- with and without Revert_Bytes depending on build-target being
-- Little- or BigEndian code

with Ada.Text_IO;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;
with Interfaces;

with Mandatory;     use Mandatory; -- NAXIS_Arr needed
with Keyword_Record; use Keyword_Record; -- FIndex needed in NAXIS_Arr
with Raw_Funcs;-- use Raw_Funcs;
with File_Funcs;


package body Raw.Data_Unit is

  use SIO;
  package TIO renames Ada.Text_IO;


 --  all Data Unit read/write


function DU_Length_blks(BITPIX : in Natural; NAXISn : in NAXIS_Arr) return Positive_Count
is
  Size_bits : SIO.Count;
  BitsPerBlock : constant Positive_Count := (2880*8);
begin
   Size_bits := File_Funcs.PrimaryImage_DataSize_Bits(BITPIX, NAXISn);
   return (1 + (Size_bits - 1) / BitsPerBlock);
end DU_Length_blks;



function DU_Data_Count(NAXISn : in NAXIS_Arr) return Positive_Count
is
  Acc : Positive_Count := 1;
begin
  for I in NAXISn'Range
  loop
    Acc := Acc * NAXISn(I);
  end loop;
  return Acc;
end DU_Data_Count;




procedure Read_Data_Unit
  (File : SIO.File_Type;
  NAXISn : in NAXIS_Arr)
is
  DULength_blks : Positive_Count := DU_Length_blks(T'Size,NAXISn);
  -- FIXME crosscheck use of T'Size instead of BITPIX, ok?
  First : NAXIS_Arr := NAXISn;
  Block : T_Data_Block;
begin

  for I in 1 .. DULength_blks
  loop
    Read_Array(File, Block);
    Data(Block);
  end loop;

end Read_Data_Unit;





procedure Write_Data_Unit
  (File : SIO.File_Type;
  NAXISn : in NAXIS_Arr)
is
  DULength : Positive_Count := DU_Data_Count(NAXISn);
  DULength_blks : Positive_Count := DU_Length_blks(T'Size, NAXISn);
  DataLast, PaddingFirst, PaddingLast : Positive_Count;
  -- FIXME T'Size instead of BITPIX ok?
  First : NAXIS_Arr := NAXISn; -- FIXME we don't need values only size
  Block : T_Data_Block;
begin

  TIO.Put_Line("T'Size        : " & Positive_Count'Image(T'Size));
  TIO.Put_Line("DULength      : " & Positive_Count'Image(DULength));
  TIO.Put_Line("DULength_blks : " & Positive_Count'Image(DULength_blks));

  for I in 1 .. (DULength_blks - 1)
  loop
    Data(Block);
    Write_Array(File, Block);
  end loop;

  -- last block handle separately because needs padding
  DataLast := 1 + ((DULength - 1) rem (2880/(T'Size/8)));
  Data(Block(1..DataLast));

  if( DataLast < (2880/(T'Size/8)) )
  then
    PaddingFirst := DataLast + 1;
    PaddingLast  := 2880/(T'Size/8);-- Block end

    TIO.Put_Line("PaddingFirst  : " & Positive_Count'Image(PaddingFirst));
    TIO.Put_Line("PaddingLast   : " & Positive_Count'Image(PaddingLast));

    Block(PaddingFirst .. PaddingLast) := (others => T_DataPadding);
    Write_Array(File, Block);
end if;

end Write_Data_Unit;






procedure Read_Data_Unit_By_Element
  (File : SIO.File_Type;
  NAXISn : in NAXIS_Arr)
is
  DULength_blks : Positive_Count := DU_Length_blks(T'Size,NAXISn);
  -- FIXME crosscheck use of T'Size instead of BITPIX, ok?
  First : NAXIS_Arr := NAXISn;
  Block : T_Data_Block;
begin

  for I in 1 .. DULength_blks
  loop
    Read_Array(File, Block);
    for I in Block'Range
    loop
        Data_Elem(Block(I));
    end loop;
  end loop;

end Read_Data_Unit_By_Element;




end Raw.Data_Unit;

