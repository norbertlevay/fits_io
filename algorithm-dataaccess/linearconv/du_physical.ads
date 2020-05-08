

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Mandatory; use Mandatory; -- NAXIS_Arr needed

with Value;

generic
  type Tm is private;       -- type in memory (data is returned to caller in this type)
  type Tm_Arr is array (Positive_Count range <>) of Tm;
  type Tc is digits <>;     -- type in which scaling is calculated
  type Tf is private;       -- type in fits-file;

Undef_Val_In  : Tf;
Undef_Val_Out : Tm;
with function Is_Undef(Vin  : Tf ) return Boolean;
with function Found_Undef(Vin : Tf) return Tm;
with function Is_Undef(Vout : Tm) return Boolean;
with function in2c(Vin : Tf) return Tc; 
with function c2out(Vc : Tc)  return Tm;

with package TT_Value is
    new Value(Tm,Tf,Tc,Undef_Val_In, Undef_Val_Out, Is_Undef, Found_Undef, Is_Undef, in2c, c2out);
package DU_Physical is

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


end DU_Physical;

