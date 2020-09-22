
with Ada.Streams.Stream_IO;
with Numeric_Type;
with Scaling;
with Image;

generic
 with package Tf is new Numeric_Type(<>);
 with package Tm is new Numeric_Type(<>);
package Data_IO is

    package SIO renames Ada.Streams.Stream_IO;

procedure Read_Array
    (F : SIO.File_Type;
    A,B     : in Float;
    Tm_Arr  : out Tm.Numeric_Arr);

procedure Write_Array
    (F : SIO.File_Type;
    A,B     : in Float;
    Tm_Arr  : in Tm.Numeric_Arr);

end Data_IO;

