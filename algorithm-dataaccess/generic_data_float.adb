
with Ada.Unchecked_Conversion;
with Interfaces;

with FITS; -- Byte-type needed

-- DBG only with Ada.Text_IO; use Ada.Text_IO;


package body Generic_Data_Float is

   procedure Revert_Bytes( Data : in out T )
   is 
	-- FIXME review: is FITS.Bytes needed here ? prefer Interfaces.Unsigned_8 ?
     Size_Bytes : Positive := T'Size / FITS.Byte'Size;
     type Arr4xU8 is array (1..Size_Bytes) of Interfaces.Unsigned_8;

     function Data_To_Arr is
       new Ada.Unchecked_Conversion(Source => T, Target => Arr4xU8);
     function Arr_To_Data is
       new Ada.Unchecked_Conversion(Source => Arr4xU8, Target => T);

     Arr  : Arr4xU8 := Data_To_Arr(Data);
     ArrO : Arr4xU8;
   begin

     for I in Arr'Range
     loop
       ArrO(I) := Arr(1 + Size_Bytes - I); 
     end loop;

     Data := Arr_To_Data(ArrO);

   end Revert_Bytes;



function Physical(BZERO : in T; BSCALE : in T; Data : in T) return T
is
begin
  return BZERO + BSCALE * Data;
end Physical;

end Generic_Data_Float;
