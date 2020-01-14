-- TODO
-- Physical_Value type Integer: implement Signed-Unsigned conversion

with Interfaces;	use Interfaces;


-- Alternative A:
-- division by DataTypes (for Size calc only) vs DataValues (for Int-Float conversions)
--with Generic_Data_Types;
--with Generic_Value;

-- Alternative B:
-- division by Int vs Float: generic includes Value Conversions and hides Endianness (per Block)
-- misses UInt8 -> only Signed integers allowed -> how go around this ?? FIXME
-- implements twice the same code on Endianness: for INts and for FLoats - but principally 
-- Float _could_ be different on Endianness -> so accept duplications of code as special case?
with Generic_Data_Integer;
with Generic_Data_Float;


package Data_Types is

   -- data types as of FITS Standard version 3

   type Unsigned_8 is new Interfaces.Unsigned_8;
   type Integer_16 is new Interfaces.Integer_16;
   type Integer_32 is new Interfaces.Integer_32;
   type Integer_64 is new Interfaces.Integer_64;

   type Float_32   is new Interfaces.IEEE_Float_32;
   type Float_64   is new Interfaces.IEEE_Float_64;

   -- 1, Data Block definitions

   -- FIXME UInt8: implement separately Ada generic accpets only Signed
   -- or convert Unsigned -> Signed before instantiating???
   -- package UInt8 is new Generic_Data_Integer(T => Unsigned_8);
   package Int16 is new Generic_Data_Integer(T => Integer_16);
   package Int32 is new Generic_Data_Integer(T => Integer_32);
   package Int64 is new Generic_Data_Integer(T => Integer_64);

   package F32 is new Generic_Data_Float(T => Float_32);
   package F64 is new Generic_Data_Float(T => Float_64);

   -- 2, Array-Physical data conversions

   function Physical_Value_F32 is new Int32.Physical(TF => Float_32);
   function Physical_Value_F32 is new Int16.Physical(TF => Float_32);
   function Physical_Value_F64 is new Int32.Physical(TF => Float_64);
   function Physical_Value_F64 is new Int16.Physical(TF => Float_64);
   -- FIXME support all combinations ? 
   -- Standard says only Int16->Float32 is commonly used (others make no sense).

end Data_Types;

