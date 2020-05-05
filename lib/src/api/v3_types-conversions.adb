
with Ada.Unchecked_Conversion;

package body V3_Types.Conversions is

 -- Integer-Float conversions

 function "+" (R : Integer_64) return Float_64 is begin return Float_64(R); end "+";
 function "+" (R : Integer_32) return Float_64 is begin return Float_64(R); end "+";
 function "+" (R : Integer_16) return Float_32 is begin return Float_32(R); end "+";
 function "+" (R : Unsigned_8) return Float_32 is begin return Float_32(R); end "+";

 -- sign conversions

 -- FIXME check there must be better way...

 generic
  type Tin  is range <>; 
  type Tout is mod <>; 
 function Signed_To_Unsigned (R : Tin) return Tout;
 function Signed_To_Unsigned (R : Tin) return Tout
 is  
 begin
   if(R < 0) then return (Tout'Last + 1) - Tout(abs R); 
   else           return Tout(R);
   end if;
 end Signed_To_Unsigned;
  -- NOTE map Int-negative values to 'upper-half'/mid-last Unsigned range
 -- First: -1 -> 64353  Last: -32768->32768


 -- FIXME check there must be better way...

 function "+" (R : Integer_64) return Unsigned_64
 is
   function "+" is new Signed_To_Unsigned(Integer_64,Unsigned_64);
 begin
   return +R;
 end "+";


 function "+" (R : Integer_32) return Unsigned_32
 is
   function "+" is new Signed_To_Unsigned(Integer_32,Unsigned_32);
 begin
   return +R;
 end "+";

 function "+" (R : Integer_16) return Unsigned_16
 is
   function "+" is new Signed_To_Unsigned(Integer_16,Unsigned_16);
 begin
   return +R;
 end "+";
 

 function "+" (R : Unsigned_8) return Integer_8
 is  
 begin
   if(R > 127) then return -Integer_8( Unsigned_8'Last + 1 - R ); 
   else             return  Integer_8( R );
   end if;
 end "+";


end V3_Types.Conversions;

