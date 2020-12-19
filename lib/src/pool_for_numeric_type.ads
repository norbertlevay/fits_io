
with V3_Types; use V3_Types;

package Pool_For_Numeric_Type
is

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




end Pool_For_Numeric_Type;
