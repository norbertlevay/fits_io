
with Ada.Streams.Stream_IO;
with Numeric_Type;
with Scaling;
with Image;
with Endian;

--generic
-- with package Tf is new Numeric_Type(<>);
-- with package Tm is new Numeric_Type(<>);
-- with package Tm_Image is new Image(<>);--T => Tm; others => <>); 

package body Data_IO is

Uf : Tf.Numeric;
Um : Tm.Numeric; -- FIXME use Raw and Physical instead Tf and Tm
Use_Undefs : Boolean := False;

-- FIXME set undef values from Tm_Image.DM - we miss calc the "other" side Undef
procedure Set_Undefined(Ufile : in Tf.Numeric; Umem : in Tm.Numeric)
is
begin
    Uf := Ufile;
    Um := Umem;
    Use_Undefs := True;
end Set_Undefined;



-- for Read
procedure TfTm_Scaling(Af : in Tf.Numeric_Arr; Tm_Arr : out Tm.Numeric_Arr;
    A,B : in Float;
    Uf : Tf.Numeric;
    Um : Tm.Numeric)
is
 Ff, Fm : Float;
 Vf : Tf.Numeric;
 Vm : Tm.Numeric;
begin

    if(Use_Undefs)
    then

        -- data may contain undefined values
        -- if undef found, it is translated

        for I in Af'Range
        loop

            Vf := Af(I);

            if(Is_Undef(Vf,Uf))
            then
                Vm := Um;
            else

                Ff := Tf.To_Float(Vf);
                Fm := A + B * Ff;
                Vm := Tm.To_Numeric(Fm);

                if(Is_Undef(Vm,Um))
                then
                    null;-- FIXME error: Vm undefined but Vf was not.
                end if;

            end if;

            Tm_Arr(I) := Vm;

        end loop;

    else

        -- data must not have undefined values
        -- all data-values considered valid and will be scaled
        -- (no undefined-value translation)

        for I in Af'Range
        loop
            Vf := Af(I);

            Ff := Tf.To_Float(Vf);
            Fm := A + B * Ff;
            Vm := Tm.To_Numeric(Fm);

            Tm_Arr(I) := Vm;
        end loop;

    end if;

end TfTm_Scaling;



-- for Write
procedure TmTf_Scaling(Tm_Arr : in Tm.Numeric_Arr; Af : out Tf.Numeric_Arr;
    A,B : in Float;
    Uf : Tf.Numeric;
    Um : Tm.Numeric)
is
 Ff, Fm : Float;
 Vf : Tf.Numeric;
 Vm : Tm.Numeric;
begin

    -- Linear

    if(Use_Undefs)
    then

        -- data may have undefined-values

        for I in Af'Range
        loop

            Vm := Tm_Arr(I);

            if(Is_Undef(Vm,Um))
            then
                Vf := Uf;
            else

                Ff := Tm.To_Float(Vm);
                Fm := A + B * Ff;
                Vf := Tf.To_Numeric(Fm);

                if(Is_Undef(Vf,Uf))
                then
                    null;-- FIXME error: Vm undefined but Vf was not.
                end if;

            end if;

            Af(I) := Vf;

        end loop;

    else

        -- no undefined-values in data

        for I in Af'Range
        loop
            Fm    := Tm.To_Float(Tm_Arr(I));
            Ff    := A + B * Fm;
            Af(I) := Tf.To_Numeric(Fm);
        end loop;

    end if;

end TmTf_Scaling;




procedure Read_Array
    (F : SIO.File_Type;
    Im : in Tm_Image.Data_Model;
    Tm_Arr : out Tm.Numeric_Arr)
is
 Af : Tf.Numeric_Arr(Tm_Arr'Range);
 procedure CheckAndRevert is new Endian.Check_And_Revert(Tf.Numeric,Tf.Numeric_Arr);
 A,B : Float; -- FIXME set from Image.Data_Model (which corresponds to Header)
begin
    Tf.Numeric_Arr'Read(SIO.Stream(F), Af);
    CheckAndRevert(Af);
    TfTm_Scaling(Af,Tm_Arr,A,B,Uf,Um);
end Read_Array;



procedure Write_Array
    (F : SIO.File_Type;
    Im : in Tm_Image.Data_Model;
    Tm_Arr : in Tm.Numeric_Arr)
is
 Af : Tf.Numeric_Arr(Tm_Arr'Range);
 procedure CheckAndRevert is new Endian.Check_And_Revert(Tf.Numeric,Tf.Numeric_Arr);
 A,B : Float; -- FIXME set from Image.Data_Model (which corresponds to Header)
begin
    TmTf_Scaling(Tm_Arr,Af,A,B,Uf,Um);
    CheckAndRevert(Af);
    Tf.Numeric_Arr'Write(SIO.Stream(F), Af);
end Write_Array;




end Data_IO;

