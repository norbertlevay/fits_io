
with Ada.Text_IO;

with Numeric_Type;
with Array_IO;

with Pool_For_Numeric_Type; use Pool_For_Numeric_Type;

package body FITS_IO is

--    type T_Arr is array(SIO.Positive_Count range <>) of T;
    type Float_Arr is array(SIO.Positive_Count range <>) of Float;


procedure Read
    (F : SIO.File_Type;
    TArr : out T_Arr)
is

    package Phys is new Numeric_Type(T,T_Arr,Float_Arr);
    package Raw  is new Numeric_Type(Float,Float_Arr,Float_Arr);
    -- FIXME type is determined by BITPIX after having read the Header
    -- now assume Float...
    package AIO  is new Array_IO(Raw,Phys);

    A,B : Float;
    Ph_Arr : Phys.Numeric_Arr(TArr'Range);
begin
    -- assume Header was read, we have A,B and Undef
    -- if BLANK in Header -> set Undef into Raw
    A := 0.0;
    B := 10.0;
    AIO.Read(F, A,B, Ph_Arr);

    for I in Ph_Arr'Range
    loop
        Ada.Text_IO.Put_Line("DBG " & Float'Image(Phys.To_Float(Ph_Arr(I))));
        TArr(I) := Ph_Arr(I);
    end loop;

end Read;


procedure Write
    (F : SIO.File_Type;
    TArr : in T_Arr)
is
    A,B : Float;

    package Phys is new Numeric_Type(T,T_Arr,Float_Arr);
    package Raw  is new Numeric_Type(T,T_Arr,Float_Arr);
    package AIO  is new Array_IO(Raw,Phys);

begin
    -- assume Header was read, we have A,B and Undef
    null;
end Write;



end FITS_IO;
