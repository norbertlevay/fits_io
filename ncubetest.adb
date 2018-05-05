
with ncube;


procedure ncubetest
is
 Offset    :  Positive := 1;
 MaxCoords :  NCube.MyVector := (3,4,5);
 Coords    :  NCube.MyVector := (9,9,9);
begin

 NCube.To_Coords (Offset, MaxCoords, Coords);

end ncubetest;



