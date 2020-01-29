
with Generic_Data_Types;

generic
  type T is digits <>; -- any floating point type
package Generic_Data_Float is

 package Data is new Generic_Data_Types( T => T);
 -- Data Block def and endianness

 function Physical_Value(BZERO : in T; BSCALE : in T; Array_Value : in T) return T; 
 -- Phys-Arr value conversions
    
end Generic_Data_Float;

