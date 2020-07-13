


package body Pool_V3Type_Convs is

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


end Pool_V3Type_Convs;
