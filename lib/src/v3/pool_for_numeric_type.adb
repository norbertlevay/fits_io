
with Ada.Text_IO;
use Ada.Text_IO;

with V3_Types; use V3_Types;

package body Pool_For_Numeric_Type
is

function "+"(V : in ABFloat) return Long_Long_Float is begin return Long_Long_Float(V); end "+";
function "+"(V : in ABFloat) return Long_Float is begin return Long_Float(V); end "+";
function "+"(V : in ABFloat) return Float      is begin return Float(V);      end "+";
function "+"(V : in ABFloat) return Integer    is begin return Integer(V);    end "+";
function "+"(V : in ABFloat) return Short_Integer    is begin return Short_Integer(V);    end "+";
function "+"(V : in ABFloat) return Short_Short_Integer is begin return Short_Short_Integer(V);    end "+";

function "+"(V : in Short_Short_Integer) return ABFloat is begin return ABFloat(V); end "+";
function "+"(V : in Short_Integer)     return ABFloat is begin return ABFloat(V); end "+";
function "+"(V : in Integer)           return ABFloat is begin return ABFloat(V); end "+";
function "+"(V : in Long_Float)        return ABFloat is begin return ABFloat(V); end "+";
function "+"(V : in Long_Long_Float)   return ABFloat is begin return ABFloat(V); end "+";

-- FITS V3 types

function "+"(R : ABFloat) return Float_64 is begin return Float_64(R); end "+";
function "+"(R : ABFloat) return Float_32 is begin return Float_32(R); end "+";
function "+"(R : ABFloat) return Integer_64 is begin return Integer_64(R); end "+";
function "+"(R : ABFloat) return Integer_32 is begin return Integer_32(R); end "+";
function "+"(R : ABFloat) return Integer_16 is begin return Integer_16(R); end "+";
function "+"(R : ABFloat) return Unsigned_8 is begin return Unsigned_8(R); end "+";

function "+"(R : Float_64) return ABFloat is begin return ABFloat(R); end "+";
function "+"(R : Float_32) return ABFloat is begin return ABFloat(R); end "+";
function "+"(R : Integer_64) return ABFloat is begin return ABFloat(R); end "+";
function "+"(R : Integer_32) return ABFloat is begin return ABFloat(R); end "+";
function "+"(R : Integer_16) return ABFloat is begin return ABFloat(R); end "+";
function "+"(R : Unsigned_8) return ABFloat is begin return ABFloat(R); end "+";


-- complementary types

function "+"(R : ABFloat) return Unsigned_64 is begin return Unsigned_64(R); end "+";
function "+"(R : ABFloat) return Unsigned_32 is begin return Unsigned_32(R); end "+";
function "+"(R : ABFloat) return Unsigned_16 is begin return Unsigned_16(R); end "+";
function "+"(R : ABFloat) return Integer_8 is begin return Integer_8(R); end "+";

function "+"(R : Unsigned_64) return ABFloat is begin return ABFloat(R); end "+";
function "+"(R : Unsigned_32) return ABFloat is begin return ABFloat(R); end "+";
function "+"(R : Unsigned_16) return ABFloat is begin return ABFloat(R); end "+";
function "+"(R : Integer_8) return ABFloat is begin return ABFloat(R); end "+";



-- FIXME use geenric packs with 'type is range <>' and 'type is digits <>'
-- to implement these

function Is_Undef(V,U : in Short_Short_Integer) return Boolean is begin return (V = U); end Is_Undef;
function Is_Undef(V,U : in Short_Integer) return Boolean is begin return (V = U); end Is_Undef;
function Is_Undef(V,U : in Integer)    return Boolean is begin return (V = U); end Is_Undef;
function Is_Undef(V,U : in Float)      return Boolean is begin return (not (V = V)); end Is_Undef;
function Is_Undef(V,U : in Long_Float) return Boolean is begin return (not (V = V)); end Is_Undef;
function Is_Undef(V,U : in Long_Long_Float) return Boolean is begin return (not (V = V)); end Is_Undef;


function To_BITPIX(V : in Short_Short_Integer) return Integer is begin return  V'Size; end To_BITPIX;
function To_BITPIX(V : in Short_Integer) return Integer is begin return  V'Size; end To_BITPIX;
function To_BITPIX(V : in Integer) return Integer is begin return  V'Size; end To_BITPIX;
function To_BITPIX(V : in Float  ) return Integer is begin return -V'Size; end To_BITPIX;
function To_BITPIX(V : in Long_Float) return Integer is begin return -V'Size; end To_BITPIX;
function To_BITPIX(V : in Long_Long_Float) return Integer is begin return -V'Size; end To_BITPIX;


function Is_Undef(V,U : in Unsigned_8) return Boolean is begin return (V = U); end Is_Undef;
function Is_Undef(V,U : in Integer_16) return Boolean is begin return (V = U); end Is_Undef;
function Is_Undef(V,U : in Integer_32) return Boolean is begin return (V = U); end Is_Undef;
function Is_Undef(V,U : in Integer_64) return Boolean is begin return (V = U); end Is_Undef;
function Is_Undef(V,U : in Float_32)   return Boolean is begin return (not (V = V)); end Is_Undef;
function Is_Undef(V,U : in Float_64)   return Boolean is begin return (not (V = V)); end Is_Undef;


function To_BITPIX(V : in Unsigned_8) return Integer is begin return  V'Size; end To_BITPIX;
function To_BITPIX(V : in Integer_16) return Integer is begin return  V'Size; end To_BITPIX;
function To_BITPIX(V : in Integer_32) return Integer is begin return  V'Size; end To_BITPIX;
function To_BITPIX(V : in Integer_64) return Integer is begin return  V'Size; end To_BITPIX;
function To_BITPIX(V : in Float_32)   return Integer is begin return  -V'Size; end To_BITPIX;
function To_BITPIX(V : in Float_64)   return Integer is begin return  -V'Size; end To_BITPIX;

-- complementary types

function Is_Undef(V,U : in Integer_8) return Boolean is begin return (V = U); end Is_Undef;
function Is_Undef(V,U : in Unsigned_16) return Boolean is begin return (V = U); end Is_Undef;
function Is_Undef(V,U : in Unsigned_32) return Boolean is begin return (V = U); end Is_Undef;
function Is_Undef(V,U : in Unsigned_64) return Boolean is begin return (V = U); end Is_Undef;


function To_BITPIX(V : in Integer_8) return Integer is begin return  V'Size; end To_BITPIX;
function To_BITPIX(V : in Unsigned_16) return Integer is begin return  V'Size; end To_BITPIX;
function To_BITPIX(V : in Unsigned_32) return Integer is begin return  V'Size; end To_BITPIX;
function To_BITPIX(V : in Unsigned_64) return Integer is begin return  V'Size; end To_BITPIX;



end Pool_For_Numeric_Type;
