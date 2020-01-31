
with Generic_Data_Block;

generic
  type T is digits <>; -- any floating point type
package Generic_Data_Float is

 package Data is new Generic_Data_Block( T => T);
 -- Data Block def and endianness

 function Min(B : in Data.Block; B_Min : in T) return T;
 function Max(B : in Data.Block; B_Max : in T) return T;

 function Physical_Value(BZERO : in T; BSCALE : in T; Array_Value : in T) return T; 
 -- Phys-Arr value conversions
    
end Generic_Data_Float;

