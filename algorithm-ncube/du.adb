
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with File_Funcs;
with Raw; use Raw;

package body DU is


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
  First : NAXIS_Arr := NAXISn; -- FIXME we don't values only size
  T_DataPadding : constant T;-- := T(0); -- FIXME how to set 0 for all types ??
  Block : T_Arr(1 .. 2880); -- FIXME explicit ranges instead of 'First .. 'First + 2880 - 1
  procedure Write_Block is new Write_Array(T, T_Arr);
begin

  for I in 1 .. (DULength_blks - 1)
  loop
    Data(Block);
    Write_Block(File, Block);
  end loop;

  -- last block handle separately because needs padding
  Data(Block);
  PaddingFirst := DULength rem (2880/(T'Size/8));
  PaddingLast  := 2880/(T'Size/8);-- Block end
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




-- access Planes


procedure Write_Planes
  (File : SIO.File_Type;
  NAXISn : in NAXIS_Arr;
  I : in Integer) -- 1 .. 999
is
  NAXISi : NAXIS_Arr := NAXISn(1 .. I);
  PlaneLength : Positive_Count := DU_Data_Count(NAXISi);
  Plane : T_Arr(1 .. PlaneLength);
  Coords : NAXIS_Arr := NAXISn((I+1) .. NAXISn'Last);
  PlaneCount : Positive_Count := DU_Data_Count(Coords);
  procedure Write_Plane is new Write_Array(T,T_Arr);
  DULength : Positive_Count := DU_Data_Count(NAXISn);
  T_DataPadding : constant T;-- := T(0); -- FIXME how to set 0 for all types ??
  PaddingLength : Positive_Count := DULength - PlaneLength*PlaneCount;
--  PaddingFirst, PaddingLast : Positive_Count;
  Padding : T_Arr := (Positive_Count(NAXISi'First) .. Positive_Count(NAXISi'First)+PaddingLength-1 => T_DataPadding);
begin

  for I in 1 .. PlaneCount
  loop
    Data(Plane);
    Write_Plane(File, Plane);
  end loop;

  -- FIXME add write padding
  Write_Plane(File, Padding);

end Write_Planes;




  --with procedure Data(PlaneCoord: in NAXIS_Arr; Plane : in T_Arr);
  -- implemented as read-by-planes
procedure Read_Planes
  (File : SIO.File_Type;
  NAXISn : in NAXIS_Arr;
  I : in Integer) -- 1 .. 999
is
  NAXISi : NAXIS_Arr := NAXISn(1 .. I);
  PlaneLength : Positive_Count := DU_Data_Count(NAXISi);
  Plane : T_Arr(1 .. PlaneLength);
  Coords : NAXIS_Arr := NAXISn((I+1) .. NAXISn'Last);
  PlaneCount : Positive_Count := DU_Data_Count(Coords);
  procedure Read_Plane is new Read_Array(T,T_Arr);
begin

  for I in 1 .. PlaneCount
  loop
    Read_Plane(File, Plane);
    Data(Plane);
  end loop;

end Read_Planes;



end DU;

