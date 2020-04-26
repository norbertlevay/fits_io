
-- FIXME  later make T_Arr private
-- such record should also store: NAXISi BITPIX, Undef_Val/BLANK


with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Mandatory; use Mandatory; -- NAXIS_Arr needed

package Raw is

 package SIO renames Ada.Streams.Stream_IO;

 generic
  type T is private;
  type T_Arr is array (Positive_Count range <>) of T;
 procedure Read_Raw_Plane
   (F : SIO.File_Type;
    Plane  : out T_Arr);


 generic
  type T is private;
  type T_Arr is array (Positive_Count range <>) of T;
 procedure Write_Raw_Plane
   (F : SIO.File_Type;
    Plane  : in T_Arr);


 generic
  type T is private;
  type T_Arr is array (Positive_Count range <>) of T;
 procedure Read_Raw_Volume
   (File : SIO.File_Type;
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    Last    : in NAXIS_Arr;
    Volume  : out T_Arr);


 generic
  type T is private;
  type T_Arr is array (Positive_Count range <>) of T;
 procedure Write_Raw_Volume
   (File : SIO.File_Type;
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    Last    : in NAXIS_Arr;
    Volume  : in T_Arr);


end Raw;

