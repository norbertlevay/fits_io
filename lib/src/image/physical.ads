
 -- FIXME  later make T_Arr private and also include
 -- NAXISn BITPIX and BLANK/UndefValue

-- NOTE:
-- with given set of actuals for Tm Tc Tf, compiler can automatically
-- substitute the actuals of funcs in generic-params if those are available:
-- * example: for V3 types prepare V3_Types.Linear V3_Types.Conversions
-- * and caller at instantiating Physical can include V3_Types.*
-- * e.g. caller can simply write:
-- *
-- * package F32I16_Physical is new Physical(Float_32, Long_Float, Integer_16);



with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Mandatory; use Mandatory; -- NAXIS_Arr needed

generic
  type Tm is private;       -- type in memory (data is returned to caller in this type)
  type Tc is digits <>;     -- type in which scaling is calculated
  type Tf is private;       -- type in fits-file;
--  with function Is_NaN(Vin : in Tf) return Boolean is <>;
  with function Linear(BZERO,BSCALE : in Tc; Vin : in Tf) return Tm is <>;
--  with function "+"(R : in Tf) return Tc is <>;
--  with function "+"(R : in Tc) return Tm is <>;
package Physical is

 package SIO renames Ada.Streams.Stream_IO;

  type Tm_Arr is array (Positive_Count range <>) of Tm;


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

