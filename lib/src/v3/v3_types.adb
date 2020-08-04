


--package body Pool_V3Type_Convs is
package body V3_Types is

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

end V3_Types;
--end Pool_String_To_V3Types;

