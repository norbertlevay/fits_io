
with Ada.Streams.Stream_IO;
with Numeric_Type;

generic
 with package Raw       is new Numeric_Type(<>);
 with package Physical  is new Numeric_Type(<>);
package Data_IO is

    package SIO renames Ada.Streams.Stream_IO;

procedure Read_Array
    (F : SIO.File_Type;
    A,B : in Float;
    Phys_Arr : out Physical.Numeric_Arr);

procedure Write_Array
    (F : SIO.File_Type;
    A,B : in Float;
    Phys_Arr : in Physical.Numeric_Arr);

end Data_IO;

