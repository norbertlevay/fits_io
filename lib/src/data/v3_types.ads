
-- Types as of FITS Standard Version 3

-- Utility package: 
-- instantiates generics for all types defined by FITS Standard Version3

with Interfaces;
with Generic_Data_Block;


package V3_Types is

   -- data types as of FITS Standard version 3

   type Unsigned_8 is new Interfaces.Unsigned_8;
   type Integer_16 is new Interfaces.Integer_16;
   type Integer_32 is new Interfaces.Integer_32;
   type Integer_64 is new Interfaces.Integer_64;

   type Float_32   is new Interfaces.IEEE_Float_32;
   type Float_64   is new Interfaces.IEEE_Float_64;

   -- complementary types (after conversion array-value -> physical-value)

   type Integer_8   is new Interfaces.Integer_8;
   type Unsigned_16 is new Interfaces.Unsigned_16;
   type Unsigned_32 is new Interfaces.Unsigned_32;
   type Unsigned_64 is new Interfaces.Unsigned_64;


   -- provides data Block type for all V3-types

   package UInt8 is new Generic_Data_Block(T => Unsigned_8);
   package Int16 is new Generic_Data_Block(T => Integer_16);
   package Int32 is new Generic_Data_Block(T => Integer_32);
   package Int64 is new Generic_Data_Block(T => Integer_64);
   package F32   is new Generic_Data_Block(T => Float_32);
   package F64   is new Generic_Data_Block(T => Float_64);
 
end V3_Types;

