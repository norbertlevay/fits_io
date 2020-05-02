
with Ada.Text_IO;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with File_Funcs;
with Raw; use Raw;

package body DU is

  package TIO renames Ada.Text_IO;

-- both DU.Create and DU.Read require File Index set at first data in DU

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


  --with procedure Data(First : in NAXIS_Arr; Block : out T_Arr);
  -- implemented as write-by-blocks
  -- Write in OUT Mode cuts the file
  -- pre-condition: File must have Mode = OUT or APPEND
procedure Write
  (File : SIO.File_Type;
  NAXISn : in NAXIS_Arr)
is
  DULength : Positive_Count := DU_Data_Count(NAXISn);
  DULength_blks : Positive_Count := DU_Length_blks(T'Size, NAXISn);
  PaddingFirst, PaddingLast : Positive_Count;
  -- FIXME T'Size instead of BITPIX ok?
  First : NAXIS_Arr := NAXISn; -- FIXME we don't need values only size
  Block : T_Arr(1 .. 2880/(T'Size/8));
  -- FIXME explicit ranges instead of 'First .. 'First + 2880 - 1
  procedure Write_Block is new Write_Array(T, T_Arr);
begin

  TIO.Put_Line("T'Size        : " & Positive_Count'Image(T'Size));
  TIO.Put_Line("DULength      : " & Positive_Count'Image(DULength));
  TIO.Put_Line("DULength_blks : " & Positive_Count'Image(DULength_blks));

  for I in 1 .. (DULength_blks - 1)
  loop
    Data(Block);
    Write_Block(File, Block);
  end loop;

  -- last block handle separately because needs padding
  Data(Block);
  PaddingFirst := DULength rem (2880/(T'Size/8));
  PaddingLast  := 2880/(T'Size/8);-- Block end

  TIO.Put_Line("PaddingFirst  : " & Positive_Count'Image(PaddingFirst));
  TIO.Put_Line("PaddingLast   : " & Positive_Count'Image(PaddingLast));

  Block(PaddingFirst .. PaddingLast) := (others => T_DataPadding);
  Write_Block(File, Block);

end Write;



procedure Read
  (File : SIO.File_Type;
  NAXISn : in NAXIS_Arr)
is
  DULength_blks : Positive_Count := DU_Length_blks(T'Size,NAXISn);
  -- FIXME crosscheck use of T'Size instead of BITPIX, ok?
  First : NAXIS_Arr := NAXISn;
  Block : T_Arr(1 .. 2880); -- FIXME explicit ranges instead of 'First .. 'First + 2880 - 1
  procedure Read_Block is new Read_Array(T, T_Arr);
begin

  for I in 1 .. DULength_blks
  loop
    Read_Block(File, Block);
    Data(Block);
  end loop;

end Read;


end DU;

