
with Ada.Text_IO;
use Ada.Text_IO;

--with V3_Types; use V3_Types;

package body Pool_For_Numeric_Type
is

function "+"(V : in Float) return Long_Long_Float is begin return Long_Long_Float(V); end "+";
function "+"(V : in Float) return Long_Float is begin return Long_Float(V); end "+";
function "+"(V : in Float) return Float      is begin return Float(V);      end "+";
function "+"(V : in Float) return Integer    is begin return Integer(V);    end "+";
function "+"(V : in Float) return Short_Integer    is begin return Short_Integer(V);    end "+";
function "+"(V : in Float) return Short_Short_Integer is begin return Short_Short_Integer(V);    end "+";

function "+"(V : in Short_Short_Integer) return Float is begin return Float(V); end "+";
function "+"(V : in Short_Integer)     return Float is begin return Float(V); end "+";
function "+"(V : in Integer)           return Float is begin return Float(V); end "+";
function "+"(V : in Long_Float)        return Float is begin return Float(V); end "+";
function "+"(V : in Long_Long_Float)   return Float is begin return Float(V); end "+";


-- FIXME use geenric packs with 'type is range <>' and 'type is digits <>'
-- to implement these

function Is_Undef(V,U : in Short_Short_Integer) return Boolean is begin return (V = U); end Is_Undef;
function Is_Undef(V,U : in Short_Integer) return Boolean is begin return (V = U); end Is_Undef;
function Is_Undef(V,U : in Integer)    return Boolean is begin return (V = U); end Is_Undef;
function Is_Undef(V,U : in Float)      return Boolean is begin return (not (V = V)); end Is_Undef;
function Is_Undef(V,U : in Long_Float) return Boolean is begin return (not (V = V)); end Is_Undef;
function Is_Undef(V,U : in Long_Long_Float) return Boolean is begin return (not (V = V)); end Is_Undef;


function To_BITPIX(V : in Float  ) return Integer is begin return -V'Size; end To_BITPIX;
function To_BITPIX(V : in Long_Float) return Integer is begin return -V'Size; end To_BITPIX;
function To_BITPIX(V : in Long_Long_Float) return Integer is begin return -V'Size; end To_BITPIX;
function To_BITPIX(V : in Integer) return Integer is begin return  V'Size; end To_BITPIX;
function To_BITPIX(V : in Short_Integer) return Integer is begin return  V'Size; end To_BITPIX;
function To_BITPIX(V : in Short_Short_Integer) return Integer is begin return  V'Size; end To_BITPIX;


end Pool_For_Numeric_Type;
