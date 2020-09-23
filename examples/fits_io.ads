
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with Numeric_Type;
with Array_IO;

generic
 type T is private;
 type T_Arr is array (Positive_Count range <>) of T;

-- from Numeric_Type pool
 with function "+"(V : in T)     return Float is <>;
 with function "+"(V : in Float) return T is <>;

 with function Is_Undef(V,U : in T) return Boolean is <>;
 with function To_BITPIX(V : in T) return Integer is <>;

package FITS_IO is

    package SIO renames Ada.Streams.Stream_IO;

procedure Read
    (F : SIO.File_Type;
    TArr : out T_Arr);

procedure Write
    (F : SIO.File_Type;
    TArr : in T_Arr);


end FITS_IO;
