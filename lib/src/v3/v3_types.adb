


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



end V3_Types;

