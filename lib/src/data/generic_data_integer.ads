-- TODO
-- Physical_Value type Integer: implement Signed-Unsigned conversion [FITS Tab 11]
-- (like Physical() below but returns type T not Float

with Generic_Data_Types;

generic
  type T is range <>; -- any signed integer type
package Generic_Data_Integer is

 package Data is new Generic_Data_Types( T => T );
 -- Data Block definition and endianness

 generic
   type TF is digits <>; -- any floating point type
 function Physical(BZERO : in TF; BSCALE : in TF; BLANK : in T; Data : in T) return TF;
 -- Phys-Arr value conversions

 Undefined_Value : exception;

end Generic_Data_Integer;

