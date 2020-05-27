
with V3_Types; use V3_Types;

package Pool_V3Type_Convs is

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


function "+"(R : Float_64) return Integer_16;
function "+"(R : Float_32) return Integer_16;

end Pool_V3Type_Convs;
