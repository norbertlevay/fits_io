


package body Pool_From_String is



function To_V3Type(S : String) return Float_64 is begin return Float_64'Value(S); end To_V3Type;
function To_V3Type(S : String) return Float_32 is begin return Float_32'Value(S); end To_V3Type;
function To_V3Type(S : String) return Integer_64 is begin return Integer_64'Value(S); end To_V3Type;
function To_V3Type(S : String) return Integer_32 is begin return Integer_32'Value(S); end To_V3Type;
function To_V3Type(S : String) return Integer_16 is begin return Integer_16'Value(S); end To_V3Type;
function To_V3Type(S : String) return Unsigned_8 is begin return Unsigned_8'Value(S); end To_V3Type;




end Pool_From_String;
