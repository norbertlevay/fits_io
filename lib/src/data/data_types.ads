-- TODO
-- Physical_Value type Integer: implement Signed-Unsigned conversion

with Interfaces;	--use Interfaces;
with Generic_Data_Block;

with Keyword_Record; use Keyword_Record; -- FInteger needed

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

   -- complementary types (after conversion array-value -> physical-value)

   type Integer_8   is new Interfaces.Integer_8;
   type Unsigned_16 is new Interfaces.Unsigned_16;
   type Unsigned_32 is new Interfaces.Unsigned_32;
   type Unsigned_64 is new Interfaces.Unsigned_64;


   -- 1, Data Block definitions

   package UInt8 is
	package Data is new Generic_Data_Block( T => Unsigned_8 );
	function To_Signed(D : in Unsigned_8) return Integer_8;
   end UInt8;

   package Int16 is new Generic_Data_Integer(T => Integer_16);
   package Int32 is new Generic_Data_Integer(T => Integer_32);
   package Int64 is new Generic_Data_Integer(T => Integer_64);

   package F32 is new Generic_Data_Float(T => Float_32);
   package F64 is new Generic_Data_Float(T => Float_64);


   -- 2, Array-Physical data conversions

--   function F32_Physical_Value is new Int16.Physical_Value(TF => Float_32);
--   function F32_Physical_Value is new Int32.Physical_Value(TF => Float_32);
--   function F64_Physical_Value is new Int64.Physical_Value(TF => Float_64);
   -- if Tab11 applies: these serve for Singed<->Unsigned conversions
   --  (do explicit cast of the return value to Integer if needed)
   -- if not Tab11 values then: scale to store floats as integers 
   --  (NOTE: Standard says only Int16<->Float32 is commonly used)

end Data_Types;

