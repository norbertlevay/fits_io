
with Ada.Text_IO;

with FITS_IO.Serialize; use FITS_IO.Serialize;

--package body Pool_V3Type_Convs is
package body V3_Types is

   package TIO renames Ada.Text_IO;

-- cases for Tin -> Tout (no Tcalc)

function "+"(R : Float_64) return Float_64 is begin return Float_64(R); end "+";
function "+"(R : Float_32) return Float_64 is begin return Float_64(R); end "+";
function "+"(R : Integer_64) return Float_64 is begin return Float_64(R); end "+";
function "+"(R : Integer_32) return Float_64 is begin return Float_64(R); end "+";
function "+"(R : Integer_16) return Float_64 is begin return Float_64(R); end "+";
function "+"(R : Unsigned_8) return Float_64 is begin return Float_64(R); end "+";

function "+"(R : Float_64) return Float_32 is begin return Float_32(R); end "+";
function "+"(R : Float_32) return Float_32 is begin return Float_32(R); end "+";
function "+"(R : Integer_64) return Float_32 is begin return Float_32(R); end "+";
function "+"(R : Integer_32) return Float_32 is begin return Float_32(R); end "+";
function "+"(R : Integer_16) return Float_32 is begin return Float_32(R); end "+";
function "+"(R : Unsigned_8) return Float_32 is begin return Float_32(R); end "+";






--function "+"(R : Float_64) return Integer_16 is begin return Integer_16(R); end "+";
--function "+"(R : Float_32) return Integer_16 is begin return Integer_16(R); end "+";


-- cases for Tcalc: Tin -> Tcalc -> Tout

function "+"(R : Float_64) return Integer_64 is begin return Integer_64(R); end "+";
function "+"(R : Float_64) return Integer_32 is begin return Integer_32(R); end "+";
function "+"(R : Float_64) return Integer_16 is begin return Integer_16(R); end "+";
function "+"(R : Float_64) return Unsigned_8 is begin return Unsigned_8(R); end "+";

function "+"(R : Float_32) return Integer_64 is begin return Integer_64(R); end "+";
function "+"(R : Float_32) return Integer_32 is begin return Integer_32(R); end "+";
function "+"(R : Float_32) return Integer_16 is begin return Integer_16(R); end "+";
function "+"(R : Float_32) return Unsigned_8 is begin return Unsigned_8(R); end "+";


--end Pool_V3Type_Convs;



--pool_string_to_v3types.adb
--package body Pool_String_To_V3Types is

function To_V3Type(S : String) return Float_64   is begin return Float_64'Value(S);   end To_V3Type;
function To_V3Type(S : String) return Float_32   is begin return Float_32'Value(S);   end To_V3Type;

function To_V3Type(S : String) return Integer_64 is begin return Integer_64'Value(S); end To_V3Type;
function To_V3Type(S : String) return Integer_32 is begin return Integer_32'Value(S); end To_V3Type;
function To_V3Type(S : String) return Integer_16 is begin return Integer_16'Value(S); end To_V3Type;
function To_V3Type(S : String) return Integer_8  is begin return Integer_8'Value(S);  end To_V3Type;

function To_V3Type(S : String) return Unsigned_8 is begin return Unsigned_8'Value(S); end To_V3Type;

--end Pool_String_To_V3Types;


-- adds T_Ops from examples/t_ops.adb

--with V3_Types; use V3_Types;
--package body T_Ops is

    function T_First return Float_64 is begin return Float_64'First; end T_First;
    function T_First return Float_32 is begin return Float_32'First; end T_First;
    function T_First return Integer_64 is begin return Integer_64'First; end T_First;
    function T_First return Integer_32 is begin return Integer_32'First; end T_First;
    function T_First return Integer_16 is begin return Integer_16'First; end T_First;
    function T_First return Unsigned_8 is begin return Unsigned_8'First; end T_First;

    function T_Last return Float_64 is begin return Float_64'Last; end T_Last;
    function T_Last return Float_32 is begin return Float_32'Last; end T_Last;
    function T_Last return Integer_64 is begin return Integer_64'Last; end T_Last;
    function T_Last return Integer_32 is begin return Integer_32'Last; end T_Last;
    function T_Last return Integer_16 is begin return Integer_16'Last; end T_Last;
    function T_Last return Unsigned_8 is begin return Unsigned_8'Last; end T_Last;

    function T_Image(V: Float_64) return String is begin return Float_64'Image(V); end T_Image;
    function T_Image(V: Float_32) return String is begin return Float_32'Image(V); end T_Image;
    function T_Image(V: Integer_64) return String is begin return Integer_64'Image(V);end T_Image;
    function T_Image(V: Integer_32) return String is begin return Integer_32'Image(V);end T_Image;
    function T_Image(V: Integer_16) return String is begin return Integer_16'Image(V);end T_Image;
    function T_Image(V: Unsigned_8) return String is begin return Unsigned_8'Image(V);end T_Image;

    function T_Valid(V: Float_64) return Boolean is begin return V'Valid; end T_Valid;
    function T_Valid(V: Float_32) return Boolean is begin return V'Valid; end T_Valid;
    function T_Valid(V: Integer_64) return Boolean is begin return V'Valid; end T_Valid;
    function T_Valid(V: Integer_32) return Boolean is begin return V'Valid; end T_Valid;
    function T_Valid(V: Integer_16) return Boolean is begin return V'Valid; end T_Valid;
    function T_Valid(V: Unsigned_8) return Boolean is begin return V'Valid; end T_Valid;

--    function ">"(L,R : Float_64) return Boolean is begin return (L>R); end ">";
--    function ">"(L,R : Float_32) return Boolean is begin return (L>R); end ">";
--end T_Ops;





--with Ada.Text_IO;
--use Ada.Text_IO;

--with V3_Types; use V3_Types;

--package body Pool_For_Numeric_Type
--is

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



--end Pool_For_Numeric_Type;



 procedure F64Arr_Write
      (Stream : access  Ada.Streams.Root_Stream_Type'Class;
       Item : F64_Arr)
 is
    procedure T_Write is new HDU_SWrite(Float_64, F64_Arr);
 begin
    TIO.Put("S");
   T_Write(Stream, Item);
 end F64Arr_Write;


-- Difficulties with FITSv3 types vs Ada-generics:
 -- * supra-types in generics do not have "numeric" type, e.g. use private or next step is Integer (range) or Float (digits) types separately
 -- * stream attributes 'Write 'Read require first-subtype



end V3_Types;
