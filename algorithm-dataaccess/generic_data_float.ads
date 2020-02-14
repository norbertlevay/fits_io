


with Ada.Streams;

with Data_Types;

generic
  type T is digits <>; -- any floating point type
package Generic_Data_Float is

 package Data is new Data_Types( T => T);
 -- Data Block def and endianness

 function Physical(BZERO : in T; BSCALE : in T; Data : in T) return T; 
 -- Phys-Arr value conversions
    
end Generic_Data_Float;

