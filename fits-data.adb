

package body FITS.Data is

   -- find minimum and maximum value of the Float32 data array
   procedure Find_MinMax_Float32
--              (F32Arr : in  Float32Arr_Type;
              (F32Arr : in  Float32_Arr;
               Min    : out Float_32;
               Max    : out Float_32)
   is
     type MyFloat is new Float_32;
     D   : FITS.Float_32;
   begin

     Min := Float_32'Large;
     Max := Float_32'Small;

     --for D of F32Arr  Ada2012 feature
     for ix in F32Arr'Range
      loop

       D := F32Arr(ix);

       if D > Max then Max := D; end if;
       if D < Min then Min := D; end if;

     end loop;

   end Find_MinMax_Float32;



end FITS.Data;

