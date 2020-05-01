
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;



package body DU is

  package SIO renames Ada.Streams.Stream_IO;

-- both DU.Create and DU.Read require File Index set at first data in DU

 --with procedure Data(First : in NAXIS_Arr; Block : out T_Arr);
  -- implemented as write-by-blocks
  -- Write in OUT Mode cuts the file
  -- pre-condition: File must have Mode = OUT or APPEND
procedure Write
  (File : SIO.File_Type;
  NAXISn : in NAXIS_Arr)
is
  DULength_blks : Positive_Count := DU_Length(NAXISn);
  First : NAXIS_Arr(NAXISn'First .. NAXISn'Last);
  Block : is array (Positive_Count range 1 .. 2880) of T;
begin

  for I in 1 .. (DULength_blks - 1)
  loop
    First := Calc_Block_First(I);
    Data(First, Block);
    T_Arr'Write(Stream(File), Block);
  end loop;

  -- last block handle separately because needs padding
  First := Calc_Block_First(DULength_blks);
  Data(First, Block);
  Block((DataLength rem 2880/(T'Size/8)) .. 2880/(T'Size/8)) := (others => Tf_DataPadding);
  T_Arr'Write(Stream(File), Block);

end Write;





  --with procedure Data(PlaneCoord: in NAXIS_Arr; Plane : in T_Arr);
  -- implemented as read-by-planes
procedure Read
  (File : SIO.File_Type;
  NAXISn : in NAXIS_Arr;
  I : in Integer) -- 1 .. 999
is
  NAXISi : NAXIS_Arr := NAXISn(1 .. I);
  PlaneLength : Positive_Count := Calc_Size(NAXISi);
  Coords : NAXIS_Arr := NAXISn((I+1) .. NAXISn'Last);
  PlaneCount : Positive_Count := Calc_Size(Coords);
begin

  for I in 1 .. PlaneCount
  loop
    Raed_Plane(File, Plane);
    Data(Coord_From_Index(Coords, I), Plane);
  end loop;

end Read;

end DU;

