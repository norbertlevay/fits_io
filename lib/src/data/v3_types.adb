
with Ada.Unchecked_Conversion;

package body V3_Types is

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
 

 -- undefined float values use IEEE NaN : 
 -- signbit=don't care & 
 -- exponent= all ones & 
 -- fraction= any but not all zeros
 generic
  type T is digits <>;
 function Is_NaN(V : in T) return Boolean;
 function Is_NaN(V : in T) return Boolean
 is
 begin
  return ( (T'Exponent(V) = 255) AND 
	   (T'Fraction(V) /= 0.0) );
 end Is_NaN;
 
 function F64_Is_NaN(V : Float_64) return Boolean
 is
  function isnan is new Is_NaN(Float_64);
 begin
  return isnan(V);
 end F64_Is_NaN;

function F32_Is_NaN(V : Float_32) return Boolean
 is
   function isnan is new Is_NaN(Float_32);
 begin
  return isnan(V);
 end F32_Is_NaN;


end V3_Types;

