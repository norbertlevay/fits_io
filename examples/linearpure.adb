




procedure linearpure is


A : Float := 1.123;
B : Float := 1.765;

Vin  : Float := 1.0005;
Vout  : Float;
max : Integer := 1000000000;
begin


for I in  1 .. max
loop

Vout := A + B * Vin;



end loop;


end linearpure;
