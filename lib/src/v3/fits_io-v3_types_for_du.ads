



with Ada.Exceptions; use Ada.Exceptions;

with Numeric_Type;
with V3_Types; use V3_Types;
with Pool_For_Numeric_Type; use Pool_For_Numeric_Type;
with Array_IO;

with Ada.Text_IO;


package FITS_IO.V3_Types_For_DU is

   package SIO renames Ada.Streams.Stream_IO;
   package TIO renames Ada.Text_IO;

   type Float_Arr is array (Positive_Count range <>) of Float;

--   package Physical is new Numeric_Type(T, T_Arr, Float_Arr);
   -- FIXME ? T can be of native Ada-types (Long_Long_Integer, Float,...)
   -- and also one of FITS V3-types
   -- Raw can be _only_ FITS V3-type

   -- V3-types

   type U8_Arr   is array (Positive_Count range <>) of Unsigned_8;
   type I16_Arr   is array (Positive_Count range <>) of Integer_16;
   type I32_Arr   is array (Positive_Count range <>) of Integer_32;
   type I64_Arr   is array (Positive_Count range <>) of Integer_64;
   type F32_Arr   is array (Positive_Count range <>) of Float_32;
   type F64_Arr   is array (Positive_Count range <>) of Float_64;

   package U8Raw  is new Numeric_Type(Unsigned_8, U8_Arr,    Float_Arr);
   package I16Raw  is new Numeric_Type(Integer_16, I16_Arr,    Float_Arr);
   package I32Raw  is new Numeric_Type(Integer_32, I32_Arr,    Float_Arr);
   package I64Raw  is new Numeric_Type(Integer_64, I64_Arr,    Float_Arr);
   package F32Raw  is new Numeric_Type(Float_32,   F32_Arr,    Float_Arr);
   package F64Raw  is new Numeric_Type(Float_64,   F64_Arr,    Float_Arr);

--   package U8_AIO is new Array_IO(U8Raw, Physical);
--   package I16_AIO is new Array_IO(I16Raw, Physical);
--   package I32_AIO is new Array_IO(I32Raw, Physical);
--   package I64_AIO is new Array_IO(I64Raw, Physical);
--   package F32_AIO is new Array_IO(F32Raw, Physical);
--   package F64_AIO is new Array_IO(F64Raw, Physical);
-- FIXME moved to fits_io.adb

end FITS_IO.V3_Types_For_DU;

