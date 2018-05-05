
with ncube;
with Ada.Text_IO;
use Ada.Text_IO;


procedure ncubetest
is
 Offset    :  Positive := 12;
 MaxCoords :  NCube.MyVector := (3,4,5);
 Coords    :  NCube.MyVector := (9,9,9);
begin

 NCube.To_Coords (Offset, MaxCoords, Coords);

 Put(Positive'Image(Offset) & ": ");
 for I in Coords'Range
 loop
  Put(" " & Positive'Image(Coords(I)));
 end loop;


end ncubetest;



