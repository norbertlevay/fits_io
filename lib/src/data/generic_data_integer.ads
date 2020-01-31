-- TODO
-- Physical_Value type Integer: implement Signed-Unsigned conversion [FITS Tab 11]
-- (like Physical() below but returns type T not Float

with Generic_Data_Block;

generic
  type T is range <>; -- any signed integer type
package Generic_Data_Integer is

 package Data is new Generic_Data_Block( T => T );
 -- Data Block definition and endianness

 function Min(B : in Data.Block; B_Min : in T) return T;
 function Max(B : in Data.Block; B_Max : in T) return T;

 generic
   type TF is digits <>; -- any floating point type
 function Physical_Value(BZERO : in TF; BSCALE : in TF; BLANK : in T; Array_Value : in T) return TF;
 -- Phys-Arr value conversions

 Undefined_Value : exception;

end Generic_Data_Integer;

