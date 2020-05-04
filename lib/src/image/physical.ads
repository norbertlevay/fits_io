
 -- FIXME  later make T_Arr private and also include
 -- NAXISn BITPIX and BLANK/UndefValue

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Mandatory; use Mandatory; -- NAXIS_Arr needed

 generic
  type Tf is private;
  type Tm is private;
  type Tm_Arr is array (Positive_Count range <>) of Tm;
  type Tc is digits <>;
  with function Linear(BZERO,BSCALE : in Tc; Vin : in Tf) return Tm;
  with function "+"(R : in Tf) return Tc is <>;
  with function "+"(R : in Tc) return Tm is <>;
package Physical is

 package SIO renames Ada.Streams.Stream_IO;


-- sequential access


procedure Read_Array
   (F : SIO.File_Type;
    BZERO, BSCALE : in Tc;
    Data : out Tm_Arr);


 procedure Write_Array
   (F : SIO.File_Type;
    BZERO, BSCALE : in Tc;
    Data : in Tm_Arr);


-- random access


 procedure Read_Volume
   (File : SIO.File_Type;
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    Last    : in NAXIS_Arr;
    BZERO, BSCALE : in Tc;
    Volume  : out Tm_Arr);-- FIXME Volume'Length must match with (Last - First)


end Physical;

