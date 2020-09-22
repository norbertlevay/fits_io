
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






procedure Read_Array
    (F : SIO.File_Type;
    Im : in Tm_Image.Data_Model;
    Tm_Arr : out Tm.Numeric_Arr)
is
 Af : Tf.Numeric_Arr(Tm_Arr'Range);
 Fin_Arr  : Tf.Float_Arr(Tm_Arr'Range); -- FIXME uses 3x more space (Af Fin Fout arrays): price
 Fout_Arr : Tm.Float_Arr(Tm_Arr'Range); -- for saving if(Use_Undef AND Is_Undef()) at cycle twice
 procedure CheckAndRevert is new Endian.Check_And_Revert(Tf.Numeric,Tf.Numeric_Arr);
 A,B : Float; -- FIXME set from Image.Data_Model (which corresponds to Header)
begin

    Tf.Numeric_Arr'Read(SIO.Stream(F), Af);
    CheckAndRevert(Af);

    --    TfTm_Scaling(Af,Tm_Arr,A,B,Uf,Um);

    Fin_Arr := Tf.To_Float(Af);
    for I in Fin_Arr'Range
    loop
        Fout_Arr(I) := A + B * Fin_Arr(I);
    end loop;

    -- calc target-domain's Undef Value

    if(Tf.Is_Undefined_Valid)
    then
        Tm.Set_Undefined(Tm."+"(A + B * Tf."+"(Tf.Get_Undefined)));
    end if;

    Tm_Arr := Tm.To_Numeric(Fout_Arr);

end Read_Array;



procedure Write_Array
    (F : SIO.File_Type;
    Im : in Tm_Image.Data_Model;
    Tm_Arr : in Tm.Numeric_Arr)
is
 Af : Tf.Numeric_Arr(Tm_Arr'Range);
 Fin_Arr  : Tm.Float_Arr(Tm_Arr'Range);
 Fout_Arr : Tf.Float_Arr(Tm_Arr'Range);
procedure CheckAndRevert is new Endian.Check_And_Revert(Tf.Numeric,Tf.Numeric_Arr);
 A,B : Float; -- FIXME set from Image.Data_Model (which corresponds to Header)
begin

    --    TmTf_Scaling(Tm_Arr,Af,A,B,Uf,Um);

    Fin_Arr := Tm.To_Float(Tm_Arr);
    for I in Fin_Arr'Range
    loop
        Fout_Arr(I) := A + B * Fin_Arr(I);
    end loop;

    -- calc target-domain's Undef Value

    if(Tm.Is_Undefined_Valid)
    then
        Tf.Set_Undefined(Tf."+"(A + B * Tm."+"(Tm.Get_Undefined)));
    end if;

    Af := Tf.To_Numeric(Fout_Arr);

    -- low-level write
    CheckAndRevert(Af);
    Tf.Numeric_Arr'Write(SIO.Stream(F), Af);

end Write_Array;




end Data_IO;

