
with Ada.Text_IO;
use Ada.Text_IO;

with V3_Types; use V3_Types;

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

procedure Scale_AB(V : Float; Raw_BITPIX : Integer; A : out Float; B : out Float)
is
begin
   A := 0.0; B := 1.0;
end Scale_AB;

generic
 type M is mod <>;
procedure Scale_AB(V : M; Raw_BITPIX : Integer; A : out Float; B : out Float);
procedure Scale_AB(V : M; Raw_BITPIX : Integer; A : out Float; B : out Float)
is
begin
   B := 1.0;
   -- [FITSv3 Tab11, Sect 4.4.2.5]
   case(Raw_BITPIX) is
      when   8 => A :=                -128.0; -- -2** 7
      when  16 => A :=               32768.0; --  2**15
      when  32 => A :=          2147483648.0; --  2**31
      when  64 => A := 9223372036854775808.0; --  2**63
      when -32 => A := 0.0;
      when -64 => A := 0.0;
      when others => null;
   end case;
end Scale_AB;

procedure Scale_AB is new Scale_AB(Unsigned_64);
procedure Scale_AB is new Scale_AB(Unsigned_32);
procedure Scale_AB is new Scale_AB(Unsigned_16);
procedure Scale_AB is new Scale_AB(Integer_8);


end Pool_For_Numeric_Type;
