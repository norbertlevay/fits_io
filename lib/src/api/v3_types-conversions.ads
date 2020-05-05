
-- Types as of FITS Standard Version 3

-- Utility package: 
-- instantiates generics for all types defined by FITS Standard Version3

with Interfaces;


package V3_Types.Conversions is

 -- integer -> float converions

 function "+" (R : Integer_64) return Float_64;
 function "+" (R : Integer_32) return Float_64;
 function "+" (R : Integer_16) return Float_32;
 function "+" (R : Unsigned_8) return Float_32;

 -- sign conversions

 function "+" (R : Integer_64) return Unsigned_64;
 function "+" (R : Integer_32) return Unsigned_32;
 function "+" (R : Integer_16) return Unsigned_16;
 function "+" (R : Unsigned_8) return Integer_8;

end V3_Types.Conversions;

