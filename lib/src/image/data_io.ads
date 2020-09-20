
with Ada.Streams.Stream_IO;
with Numeric_Type;
with Scaling;
with Image;

generic
 with package Tf is new Numeric_Type(<>);
 with package Tm is new Numeric_Type(<>);

 with package Tm_Image is new Image(<>);  --T => Tm; others => <>);
 -- we must have read/created Header before doing Data_IO
 -- Image.Data_Model and Header are equivalent

 with function Is_Undef(V,U : in Tf.Numeric) return Boolean is <>;
 with function Is_Undef(V,U : in Tm.Numeric) return Boolean is <>;

package Data_IO is

    package SIO renames Ada.Streams.Stream_IO;

procedure Read_Array
    (F : SIO.File_Type;
    Im : in Tm_Image.Data_Model;
    Tm_Arr : out Tm.Numeric_Arr);

procedure Write_Array
    (F : SIO.File_Type;
    Im : in Tm_Image.Data_Model;
    Tm_Arr : in Tm.Numeric_Arr);

end Data_IO;

