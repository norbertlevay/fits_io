
with Ada.Streams.Stream_IO;
with Numeric_Type;
with Endian;

-- FIXME stack/local arrays: Af Fin_Arr Fout_Arr Af, use 3x more space: price to pay
-- for saving if(Use_Undef AND Is_Undef()) at cycle twice


package body Array_IO is

package Phys renames Physical;

procedure Read
    (F : SIO.File_Type;
    A,B : in Float;
    Phys_Arr : out Physical.Numeric_Arr)
is
 Af         : Raw.Numeric_Arr(Phys_Arr'Range);
 Fin_Arr    : Raw.Float_Arr(Phys_Arr'Range);
 Fout_Arr   : Phys.Float_Arr(Phys_Arr'Range);
 procedure CheckAndRevert is new Endian.Check_And_Revert(Raw.Numeric,Raw.Numeric_Arr);
begin

    -- low-level read

    Raw.Numeric_Arr'Read(SIO.Stream(F), Af);
    CheckAndRevert(Af);

    -- conversions

    Fin_Arr := Raw.To_Float(Af);
    for I in Fin_Arr'Range
    loop
        Fout_Arr(I) := A + B * Fin_Arr(I);
    end loop;

    -- calc target-domain's Undef Value
    if(Raw.Is_Undefined_Valid)
    then
        Phys.Set_Undefined(Phys."+"(A + B * Raw."+"(Raw.Get_Undefined)));
    end if;

    Phys_Arr := Phys.To_Numeric(Fout_Arr);

end Read;



procedure Write
    (F : SIO.File_Type;
    A,B : in Float;
    Phys_Arr : in Physical.Numeric_Arr)
is
 Af         : Raw.Numeric_Arr(Phys_Arr'Range);
 Fin_Arr    : Phys.Float_Arr(Phys_Arr'Range);
 Fout_Arr   : Raw.Float_Arr(Phys_Arr'Range);
procedure CheckAndRevert is new Endian.Check_And_Revert(Raw.Numeric,Raw.Numeric_Arr);
begin

    -- conversions

    Fin_Arr := Phys.To_Float(Phys_Arr);
    for I in Fin_Arr'Range
    loop
        Fout_Arr(I) := A + B * Fin_Arr(I);
    end loop;

    -- calc target-domain's Undef Value
    if(Phys.Is_Undefined_Valid)
    then
        Raw.Set_Undefined(Raw."+"(A + B * Phys."+"(Phys.Get_Undefined)));
    end if;

    Af := Raw.To_Numeric(Fout_Arr);

    -- low-level write

    CheckAndRevert(Af);
    Raw.Numeric_Arr'Write(SIO.Stream(F), Af);

end Write;


end Array_IO;

