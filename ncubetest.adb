
with FITS.Size;
use  FITS.Size;

with ncube;
with Ada.Text_IO;
use Ada.Text_IO;


procedure ncubetest
is
 Offset    :  FPositive := 12;
-- MaxCoords :  NCube.MyVector := (3,4,5,others => 1);
 MaxCoords :  NCube.Coord_Type := (3,4,5,others => 1);
-- Coords    :  NCube.MyVector := (9,9,9,others => 1);
 Coords    :  NCube.Coord_Type := (9,9,9,others => 1);
begin

 NCube.To_Coords (Offset, MaxCoords, Coords);

 Put(FPositive'Image(Offset) & ": ");
 for I in Coords'Range
 loop
  Put(" " & FPositive'Image(Coords(I)));
 end loop;


end ncubetest;



