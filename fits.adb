
--with Ada.Text_IO; -- debug only

package body FITS is


   --
   -- convert BITPIX keyword from Header to internal FitsData_Type
   --
   function  To_FitsDataType (BITPIX : in Integer ) return FitsData_Type
   is
    bp : FitsData_Type;
   begin
    case BITPIX is
    when   8 => bp := Int8;
    when  16 => bp := Int16;
    when  32 => bp := Int32;
    when  64 => bp := Int64;
    when -32 => bp := Float32;
    when -64 => bp := Float64;
    when others =>
     null;
     -- FIXME ? raise exception "out of range"
    end case;
    return bp;
   end To_FITSDataType;

end FITS;

