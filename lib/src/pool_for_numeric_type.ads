
--with V3_Types; use V3_Types;

package Pool_For_Numeric_Type
is

function "+"(V : in Float) return Long_Long_Float;
function "+"(V : in Float) return Long_Float;
function "+"(V : in Float) return Float;
function "+"(V : in Float) return Integer;
function "+"(V : in Float) return Short_Integer;
function "+"(V : in Float) return Short_Short_Integer;

function "+"(V : in Short_Short_Integer)           return Float;
function "+"(V : in Short_Integer)           return Float;
function "+"(V : in Integer)           return Float;
function "+"(V : in Long_Float)        return Float;
function "+"(V : in Long_Long_Float)   return Float;

function Is_Undef(V,U : in Short_Short_Integer)    return Boolean;
function Is_Undef(V,U : in Short_Integer)    return Boolean;
function Is_Undef(V,U : in Integer)    return Boolean;
function Is_Undef(V,U : in Float)      return Boolean;
function Is_Undef(V,U : in Long_Float) return Boolean;
function Is_Undef(V,U : in Long_Long_Float) return Boolean;


function To_BITPIX(V : in Float  ) return Integer;
function To_BITPIX(V : in Long_Float) return Integer;
function To_BITPIX(V : in Long_Long_Float) return Integer;
function To_BITPIX(V : in Integer) return Integer;
function To_BITPIX(V : in Short_Integer) return Integer;
function To_BITPIX(V : in Short_Short_Integer) return Integer;



end Pool_For_Numeric_Type;
