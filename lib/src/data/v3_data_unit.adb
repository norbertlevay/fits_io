


package body V3_Data_Unit is

 -- Integer-Float conversions

 function "+" (R : Integer_64) return Float_64 is begin return Float_64(R); end "+";
 function "+" (R : Integer_32) return Float_64 is begin return Float_64(R); end "+";
 function "+" (R : Integer_16) return Float_32 is begin return Float_32(R); end "+";
 function "+" (R : Unsigned_8) return Float_32 is begin return Float_32(R); end "+";

 -- sign conversions
 
 function "+" (R : Integer_16) return Unsigned_16
 is  
 begin
   if(R < 0) then return (Unsigned_16'Last + 1) - Unsigned_16(abs R); 
   else           return Unsigned_16(R);
   end if;
 end "+";
 -- NOTE map Int-negative values to 'upper-half'/mid-last Unsigned range
 -- First: -1 -> 64353  Last: -32768->32768

end V3_Data_Unit;

