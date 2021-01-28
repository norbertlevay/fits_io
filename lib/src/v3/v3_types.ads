
-- Types as of FITS Standard Version 3

-- Utility package: 
-- instantiates generics for all types defined by FITS Standard Version3

with Interfaces;

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





end V3_Types;

