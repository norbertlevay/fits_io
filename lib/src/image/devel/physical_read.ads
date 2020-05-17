
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


-- FIXME: API should support writing generic algorithms which will cover all
-- combinations of type sets Tf and Tm -> e.g. Ada-compiler will expand such user-code
-- Example: for V3-types, would end up in 60 combinations 10 x 6:
-- Tf: F64 F32 I64 .. I16 U8  ----->  6
-- Tm: F64 F32 I64 ...I8 U64...U8 -> 10

-- NOTE API:
-- Physical and FF FI ... modules are the most general: 
--  allow instantiation of whatever numeric types -> generic params: Tm Tc Tf
-- api-V3: resolve all file-types:              generic params left: Tm Tc
-- api-V3/CPI and implement some Tm type-set:   generic params left: Tc
-- Last api-V3/CPU allows to trade precision vs CPU-speed for an algorithm already
-- implemented.

-- FIXME in Read_Volume: Volume'Length must match with (Last - First)

-- NOTE in Linear below: for Write swap Tf<->Tm


with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Mandatory; use Mandatory; -- NAXIS_Arr needed


generic
  type Tm is private;   -- type in memory
  type Tm_Arr is array (Positive_Count range <>) of Tm;
  type Tc is digits <>; -- type in which scaling is calculated
  type Tf is private;   -- type in fits-file
with function Linear(Vin : in Tf; A,B:Tm; BV : Boolean; BLANK : Tf) return Tm is <>;
package Physical_Read is

 package SIO renames Ada.Streams.Stream_IO;



     procedure Read_Array
         (F : SIO.File_Type;
         Data : out Tm_Arr;
         A,B:Tm; BV : Boolean; BLANK : Tf);



     subtype Tm_Data_Block is Tm_Arr(1 .. 2880/(Tm'Size/8));
     generic
     with procedure Data(Block : in Tm_Data_Block);
     procedure Read_All_Data_Unit
         (File : SIO.File_Type;
         NAXISn : in NAXIS_Arr;
         A,B:Tm; BV : Boolean; BLANK : Tf);



     procedure Read_Volume
         (File : SIO.File_Type;
         DUStart : in Positive_Count;
         NAXISn  : in NAXIS_Arr;
         First   : in NAXIS_Arr;
         Last    : in NAXIS_Arr;
         Volume  : out Tm_Arr;
         A,B:Tm; BV : Boolean; BLANK : Tf);



end Physical_Read;

