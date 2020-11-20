

with FITS_IO; use FITS_IO;


generic
type T is private;

with function "+"(V : in Float) return T     is <>;
with function "+"(V : in T)     return Float is <>;
with function Is_Undef  (V,U : in T) return Boolean is <>;
with function To_BITPIX (V   : in T) return Integer is <>;

package FITS_IO.Data_Unit is

   type T_Arr is array (Positive_Count range <>) of T;

  procedure Read
     (File : File_Type;
      DU   : Data_Unit_Type;
      Item : out T_Arr;
      Last : out Count);


  procedure Write
     (File : File_Type;
      DU   : Data_Unit_Type;
      Item : T_Arr);


end FITS_IO.Data_Unit;

