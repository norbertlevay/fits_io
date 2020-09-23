

-- FITS file format (may) store data in different
-- format/type as the one in which the data is provided
-- and also allows to reserve one value to mark undefined
-- values in the array


-- FIXME due to [1] Undef value it is not possible to use 
-- type T (<>) is private; which would allow also Arrays
-- Would be trouble anyway [2] as we cannot know is actual
-- param array or primitive type and we'd probably need to know
-- this at some part of the code

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with Numeric_Type;
with Array_IO;


generic
 type T is private;

 with function "+"(V : in T) return Float is <>;
 with function "+"(V : in Float) return T is <>;
 with function Is_Undef(V,U : in T) return Boolean is <>;

package FITS_IO is

    package SIO renames Ada.Streams.Stream_IO;

procedure Read(F : SIO.File_Type; V : out T);
procedure Write(F : SIO.File_Type; V : in T);


end FITS_IO;
