
with V3_Types; use V3_Types;


package Pool_String_To_V3Types is

function To_V3Type(S : String) return Float_64;
function To_V3Type(S : String) return Float_32;

function To_V3Type(S : String) return Integer_64;
function To_V3Type(S : String) return Integer_32;
function To_V3Type(S : String) return Integer_16;
function To_V3Type(S : String) return Integer_8;

function To_V3Type(S : String) return Unsigned_8;

end Pool_String_To_V3Types;

