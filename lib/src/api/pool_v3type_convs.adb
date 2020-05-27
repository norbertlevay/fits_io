


package body Pool_V3Type_Convs is

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






function "+"(R : Float_64) return Integer_16 is begin return Integer_16(R); end "+";
function "+"(R : Float_32) return Integer_16 is begin return Integer_16(R); end "+";


end Pool_V3Type_Convs;
