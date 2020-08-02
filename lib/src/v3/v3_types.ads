
-- Types as of FITS Standard Version 3

-- Utility package: 
-- instantiates generics for all types defined by FITS Standard Version3

with Interfaces;


package V3_Types is

   -- data types as of FITS Standard version 3

   type Unsigned_8 is new Interfaces.Unsigned_8;
   type Integer_16 is new Interfaces.Integer_16;
   type Integer_32 is new Interfaces.Integer_32;
   type Integer_64 is new Interfaces.Integer_64;

   type Float_32   is new Interfaces.IEEE_Float_32;
   type Float_64   is new Interfaces.IEEE_Float_64;

   F64Zero :Float_64 := 0.0;
   F32Zero :Float_32 := 0.0;
   F64NaN : Float_64 := 0.0/F64Zero;
   F32NaN : Float_32 := 0.0/F32Zero;
--   F64NaN : constant Float_64 := Float_64(16#7FF0000000000100#);
--   F32NaN : constant Float_32 := Float_32(16#7F800001#);
-- FIXME why the hexa constants were not ok ??

   -- complementary types (after conversion array-value -> physical-value)

   type Integer_8   is new Interfaces.Integer_8;
   type Unsigned_16 is new Interfaces.Unsigned_16;
   type Unsigned_32 is new Interfaces.Unsigned_32;
   type Unsigned_64 is new Interfaces.Unsigned_64;

end V3_Types;
