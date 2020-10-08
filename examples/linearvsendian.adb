

with Interfaces;


procedure linearvsendian is


A : Float := 1.123;
B : Float := 1.765;

Vin  : Float := 1.0005;
Vout  : Float;
max : Integer := 1000000000;


barr : array (1 .. 4) of Interfaces.Unsigned_8;



begin


for I in  1 .. max
loop

Vout := A + B * Vin;

for I in barr'range
loop
    barr(I) := Barr(1 + 4-I);
end loop;



end loop;


end linearvsendian;
