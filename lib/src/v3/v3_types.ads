
-- Types as of FITS Standard Version 3

-- Utility package: 
-- instantiates generics for all types defined by FITS Standard Version3

with Interfaces;
with Ada.Streams;
--with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Arrays need Positive_Count
with FITS; use FITS;


package V3_Types is

   -- data types as of FITS Standard version 3

   type Unsigned_8 is new Interfaces.Unsigned_8;
   type Integer_16 is new Interfaces.Integer_16;
   type Integer_32 is new Interfaces.Integer_32;
   type Integer_64 is new Interfaces.Integer_64;

   type Float_32   is new Interfaces.IEEE_Float_32;
   type Float_64   is new Interfaces.IEEE_Float_64;
--   type Float_64   is digits 15;
--   for Float_64'Size use 64;

   procedure F64_Write
      (Stream : access  Ada.Streams.Root_Stream_Type'Class;
      Item : Float_64) is null;-- FIXME null
   for Float_64'Write use F64_Write;


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

--end V3_Types;

-- v3_arrays.ads
--with V3_Types; use V3_Types;

--with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;


--package V3_Arrays is

    -- NOTE dependent on Positive-Count -> make generic by Index_Type ??
    type F64_Arr is array (Positive_Count range <>) of Float_64;
    type F32_Arr is array (Positive_Count range <>) of Float_32;
    type I64_Arr is array (Positive_Count range <>) of Integer_64;
    type I32_Arr is array (Positive_Count range <>) of Integer_32;
    type I16_Arr is array (Positive_Count range <>) of Integer_16;
    type U8_Arr  is array (Positive_Count range <>) of Unsigned_8;


   -- Stream attribs 'Read 'Write


   use Ada.Streams;

   procedure F64Arr_Read(Stream : access  Root_Stream_Type'Class; Item : out F64_Arr);
   procedure F32Arr_Read(Stream : access  Root_Stream_Type'Class; Item : out F32_Arr);
   procedure I64Arr_Read(Stream : access  Root_Stream_Type'Class; Item : out I64_Arr);
   procedure I32Arr_Read(Stream : access  Root_Stream_Type'Class; Item : out I32_Arr);
   procedure I16Arr_Read(Stream : access  Root_Stream_Type'Class; Item : out I16_Arr);
   procedure U8Arr_Read(Stream : access  Root_Stream_Type'Class; Item : out U8_Arr);

   procedure F64Arr_Write(Stream : access  Root_Stream_Type'Class; Item : F64_Arr);
   procedure F32Arr_Write(Stream : access  Root_Stream_Type'Class; Item : F32_Arr);
   procedure I64Arr_Write(Stream : access  Root_Stream_Type'Class; Item : I64_Arr);
   procedure I32Arr_Write(Stream : access  Root_Stream_Type'Class; Item : I32_Arr);
   procedure I16Arr_Write(Stream : access  Root_Stream_Type'Class; Item : I16_Arr);
   procedure U8Arr_Write(Stream : access  Root_Stream_Type'Class; Item : U8_Arr);

   for F64_Arr'Read use F64Arr_Read;
   for F32_Arr'Read use F32Arr_Read;
   for I64_Arr'Read use I64Arr_Read;
   for I32_Arr'Read use I32Arr_Read;
   for I16_Arr'Read use I16Arr_Read;
   for U8_Arr'Read use U8Arr_Read;

   for F64_Arr'Write use F64Arr_Write;
   for F32_Arr'Write use F32Arr_Write;
   for I64_Arr'Write use I64Arr_Write;
   for I32_Arr'Write use I32Arr_Write;
   for I16_Arr'Write use I16Arr_Write;
   for U8_Arr'Write use U8Arr_Write;


--end V3_Arrays;
   -- pool_v3type_convs.ads.adb

--with V3_Types; use V3_Types;
--package Pool_V3Type_Convs is

-- cases for Tin -> Tout (no Tcalc)

function "+"(R : Float_64) return Float_64;
function "+"(R : Float_32) return Float_64;
function "+"(R : Integer_64) return Float_64;
function "+"(R : Integer_32) return Float_64;
function "+"(R : Integer_16) return Float_64;
function "+"(R : Unsigned_8) return Float_64;


function "+"(R : Float_64) return Float_32;
function "+"(R : Float_32) return Float_32;
function "+"(R : Integer_64) return Float_32;
function "+"(R : Integer_32) return Float_32;
function "+"(R : Integer_16) return Float_32;
function "+"(R : Unsigned_8) return Float_32;


--function "+"(R : Float_64) return Integer_16;
--function "+"(R : Float_32) return Integer_16;


-- cases for Tcalc: Tin -> Tcalc -> Tout

function "+"(R : Float_64) return Integer_64;
function "+"(R : Float_64) return Integer_32;
function "+"(R : Float_64) return Integer_16;
function "+"(R : Float_64) return Unsigned_8;

function "+"(R : Float_32) return Integer_64;
function "+"(R : Float_32) return Integer_32;
function "+"(R : Float_32) return Integer_16;
function "+"(R : Float_32) return Unsigned_8;



--end Pool_V3Type_Convs;




-- pool_string_to_v3types.ads
--with V3_Types; use V3_Types;
--package Pool_String_To_V3Types is

function To_V3Type(S : String) return Float_64;
function To_V3Type(S : String) return Float_32;

function To_V3Type(S : String) return Integer_64;
function To_V3Type(S : String) return Integer_32;
function To_V3Type(S : String) return Integer_16;
function To_V3Type(S : String) return Integer_8;

function To_V3Type(S : String) return Unsigned_8;

--end Pool_String_To_V3Types;


-- adds examples/t_ops.ads

--with V3_Types; use V3_Types;
--package T_Ops is

    function T_First return Float_64;
    function T_First return Float_32;
    function T_First return Integer_64;
    function T_First return Integer_32;
    function T_First return Integer_16;
    function T_First return Unsigned_8;

    function T_Last return Float_64;
    function T_Last return Float_32;
    function T_Last return Integer_64;
    function T_Last return Integer_32;
    function T_Last return Integer_16;
    function T_Last return Unsigned_8;


    function T_Image(V: Float_64) return String;
    function T_Image(V: Float_32) return String;
    function T_Image(V: Integer_64) return String;
    function T_Image(V: Integer_32) return String;
    function T_Image(V: Integer_16) return String;
    function T_Image(V: Unsigned_8) return String;

    function T_Valid(V: Float_64) return Boolean;
    function T_Valid(V: Float_32) return Boolean;
    function T_Valid(V: Integer_64) return Boolean;
    function T_Valid(V: Integer_32) return Boolean;
    function T_Valid(V: Integer_16) return Boolean;
    function T_Valid(V: Unsigned_8) return Boolean;

--    function ">"(L,R : Float_64) return Boolean;
--    function ">"(L,R : Float_32) return Boolean;
--end T_Ops;







--with V3_Types; use V3_Types;

--package Pool_For_Numeric_Type
--is

   -- conversions to/from ABFloat

   subtype ABFloat is Float;

function "+"(V : in ABFloat) return Long_Long_Float;
function "+"(V : in ABFloat) return Long_Float;
function "+"(V : in ABFloat) return Float;
function "+"(V : in ABFloat) return Integer;
function "+"(V : in ABFloat) return Short_Integer;
function "+"(V : in ABFloat) return Short_Short_Integer;

function "+"(V : in Short_Short_Integer)  return ABFloat;
function "+"(V : in Short_Integer)        return ABFloat;
function "+"(V : in Integer)              return ABFloat;
function "+"(V : in Long_Float)           return ABFloat;
function "+"(V : in Long_Long_Float)      return ABFloat;


function "+"(R : in ABFloat) return Unsigned_8;
function "+"(R : in ABFloat) return Integer_16;
function "+"(R : in ABFloat) return Integer_32;
function "+"(R : in ABFloat) return Integer_64;
function "+"(R : in ABFloat) return Float_32;
function "+"(R : in ABFloat) return Float_64;

function "+"(R : in Unsigned_8) return ABFloat;
function "+"(R : in Integer_16) return ABFloat;
function "+"(R : in Integer_32) return ABFloat;
function "+"(R : in Integer_64) return ABFloat;
function "+"(R : in Float_32)   return ABFloat;
function "+"(R : in Float_64)   return ABFloat;

   -- complementary integer types

function "+"(R : in ABFloat) return Integer_8;
function "+"(R : in ABFloat) return Unsigned_16;
function "+"(R : in ABFloat) return Unsigned_32;
function "+"(R : in ABFloat) return Unsigned_64;

function "+"(R : in Integer_8) return ABFloat;
function "+"(R : in Unsigned_16) return ABFloat;
function "+"(R : in Unsigned_32) return ABFloat;
function "+"(R : in Unsigned_64) return ABFloat;


   -- ABFloat independent ops

function Is_Undef(V,U : in Short_Short_Integer)    return Boolean;
function Is_Undef(V,U : in Short_Integer)    return Boolean;
function Is_Undef(V,U : in Integer)    return Boolean;
function Is_Undef(V,U : in Float)      return Boolean;
function Is_Undef(V,U : in Long_Float) return Boolean;
function Is_Undef(V,U : in Long_Long_Float) return Boolean;


function To_BITPIX(V : in Short_Short_Integer) return Integer;
function To_BITPIX(V : in Short_Integer) return Integer;
function To_BITPIX(V : in Integer) return Integer;
function To_BITPIX(V : in Float  ) return Integer;
function To_BITPIX(V : in Long_Float) return Integer;
function To_BITPIX(V : in Long_Long_Float) return Integer;

-- V3 FITS types

function Is_Undef(V,U : in Unsigned_8) return Boolean;
function Is_Undef(V,U : in Integer_16) return Boolean;
function Is_Undef(V,U : in Integer_32) return Boolean;
function Is_Undef(V,U : in Integer_64) return Boolean;
function Is_Undef(V,U : in Float_32)   return Boolean;
function Is_Undef(V,U : in Float_64)   return Boolean;


function To_BITPIX(V : in Unsigned_8) return Integer;
function To_BITPIX(V : in Integer_16) return Integer;
function To_BITPIX(V : in Integer_32) return Integer;
function To_BITPIX(V : in Integer_64) return Integer;
function To_BITPIX(V : in Float_32)   return Integer;
function To_BITPIX(V : in Float_64)   return Integer;

-- FITS complementary types

function Is_Undef(V,U : in Integer_8) return Boolean;
function Is_Undef(V,U : in Unsigned_16) return Boolean;
function Is_Undef(V,U : in Unsigned_32) return Boolean;
function Is_Undef(V,U : in Unsigned_64) return Boolean;


function To_BITPIX(V : in Integer_8) return Integer;
function To_BITPIX(V : in Unsigned_16) return Integer;
function To_BITPIX(V : in Unsigned_32) return Integer;
function To_BITPIX(V : in Unsigned_64) return Integer;

--end Pool_For_Numeric_Type;

end V3_Types;
