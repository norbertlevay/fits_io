
with Ada.Unchecked_Conversion;
with System;
use  System;

package body FITS.Data is

   --
   -- convert BITPIX keyword from Header to internal Data_Type
   --
   function  To_DataType (BITPIX : in Integer) return Data_Type
   is
    bp : Data_Type;
   begin
    case BITPIX is
    when   8 => bp := UInt8;
    when  16 => bp := Int16;
    when  32 => bp := Int32;
    when  64 => bp := Int64;
    when -32 => bp := Float32;
    when -64 => bp := Float64;
    when others =>
     null;
     -- FIXME raise exception "out of range"
     -- BITPIX is read from file, can be "whatever"
    end case;
    return bp;
   end To_DataType;
   -- we need to separate BITPIX and FitsData_Type definition because
   -- Ada does not allow enumeration values to be negative (as needed for FloatNM)

   -- find minimum and maximum value of the Float32 data array
   procedure Find_MinMax_Float32
              (F32Arr : in  Float32Arr_Type;
               Min    : out Float_32;
               Max    : out Float_32)
   is
     type MyFloat is new Float_32;
   begin

     Min := Float_32'Large;
     Max := Float_32'Small;

     for D of F32Arr
      loop

       if D > Max then Max := D; end if;
       if D < Min then Min := D; end if;

     end loop;

   end Find_MinMax_Float32;



end FITS.Data;

