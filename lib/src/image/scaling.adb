
with Endian;


package body Scaling is

Usrc : Tsrc.Numeric;
Udst : Tdst.Numeric;
Use_Undefs : Boolean := False;



procedure Set_Undefined(Us : in Tsrc.Numeric; Ud : in Tdst.Numeric)
is
begin
    Usrc := Us;
    Udst := Ud;--Linear(U);
    Use_Undefs := True;
end Set_Undefined;




function  Is_Undefined(V : in Tsrc.Numeric) return Boolean
is
begin
   return Is_Undef(V,Usrc);
end Is_Undefined;

function  Is_Undefined(V : in Tdst.Numeric) return Boolean
is
begin
   return Is_Undef(V,Udst);
end Is_Undefined;





function Linear(V : in Tsrc.Numeric) return Tdst.Numeric
is
    Vout : Tdst.Numeric;
    Fin  : Float;
    Fout : Float;
begin

    -- instead of Use_Undefs-flag, could use OOP/tagged-records and dispatching ->
    -- would save to repeat the else branch; however would be an overkill:
    -- there are only two cases: we have or have-not Undef values

    if(Use_Undefs)
    then

        -- data has Undefs

        if(Is_Undefined(V))
        then
            Vout := Udst;
        else

            Fin  := Tsrc.To_Float(V);
            Fout := A + B * Fin;
            Vout := Tdst.To_Numeric(Fout);

            if(Is_Undefined(Vout))
            then
                null;-- error: "Vout undefined but Vin was not."
            end if;

        end if;

    else

        -- data has no Undefs

        Fin  := Tsrc.To_Float(V);
        Fout := A + B * Fin;
        Vout := Tdst.To_Numeric(Fout);

    end if;

    return Vout;

end Linear;




procedure Linear(Ain : in Tsrc_Numeric_Arr; Aout : out Tdst_Numeric_Arr)
is
    Vin  : Tsrc.Numeric;
    Vout : Tdst.Numeric;
    Fin  : Float;
    Fout : Float;
begin

    -- instead of Use_Undefs-flag, could use OOP/tagged-records and dispatching ->
    -- would save to repeat the else branch; however would be an overkill:
    -- there are only two cases: we have or have-not Undef values

    if(Use_Undefs)
    then

        -- data has Undefs

        for I in Ain'Range
        loop

            Vin := Ain(I);

            if(Is_Undefined(Vin))
            then
                Vout := Udst;
            else

                Fin  := Tsrc.To_Float(Vin);
                Fout := A + B * Fin;
                Vout := Tdst.To_Numeric(Fout);

                if(Is_Undefined(Vout))
                then
                    null;-- error: "Vout undefined but Vin was not."
                end if;

            end if;

            Aout(I) := Vout;

        end loop;


    else

        -- data has no Undefs

        for I in Ain'Range
        loop

            Vin := Ain(I);

            Fin  := Tsrc.To_Float(Vin);
            Fout := A + B * Fin;
            Vout := Tdst.To_Numeric(Fout);

            Aout(I) := Vout;

        end loop;

    end if;

end Linear;





-- Read from file
procedure Linear(Fsrc : SIO.File_Type; Aout : out Tdst_Numeric_Arr)
is
    Ain : Tsrc_Numeric_Arr(Aout'Range);
    procedure CheckAndRevert is new Endian.Check_And_Revert(Tsrc.Numeric,Tsrc_Numeric_Arr);
begin
    Tsrc_Numeric_Arr'Read(SIO.Stream(Fsrc), Ain);
    CheckAndRevert(Ain);
    Linear(Ain,Aout);
end Linear;


-- Write to file
procedure Linear(Ain : in Tsrc_Numeric_Arr; Fdst : SIO.File_Type)
is
    Aout : Tdst_Numeric_Arr(Ain'Range);
    procedure CheckAndRevert is new Endian.Check_And_Revert(Tdst.Numeric,Tdst_Numeric_Arr);
begin
    Linear(Ain,Aout);
    CheckAndRevert(Aout);
    Tdst_Numeric_Arr'Write(SIO.Stream(Fdst), Aout);
end Linear;





end Scaling;

