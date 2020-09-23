

-- FITS file format (may) store data in different
-- format/type as the one in which the data is provided
-- and also allows to reserve one value to mark undefined
-- values in the array

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with Numeric_Type;
with Array_IO;


generic
 type T is private;
 type T_Arr is array (Positive_Count range <>) of T;

 with function "+"(V : in T) return Float is <>;
 with function "+"(V : in Float) return T is <>;
 with function Is_Undef(V,U : in T) return Boolean is <>;

package FITS_IO is

    package SIO renames Ada.Streams.Stream_IO;

procedure Read
    (F : SIO.File_Type;
    TArr : out T_Arr);

procedure Write
    (F : SIO.File_Type;
    TArr : in T_Arr);


end FITS_IO;
