


package body Physical is

    AA, BB : Tm;


procedure Read_Array(dummy : Integer)
is
    Vin  : Tf;
    Vout : Tm;
begin
    Vout := Linear(Vin,AA,BB);
end Read_Array;



end Physical;
