

--with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with FITS; use FITS;
--with FITS_IO; use FITS_IO;
-- Positive_Count needed


package Endian is



  generic
    type T is private;
  procedure Revert_Bytes( Data : in out T );



  generic
    type T is private;
    type T_Arr is array (Positive_Count range <>) of T;
  procedure Check_And_Revert(Arr : in out T_Arr);


end Endian;

