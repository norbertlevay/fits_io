
with Ada.Unchecked_Conversion;
with Interfaces;

with FITS; -- Byte-type needed

-- DBG only with Ada.Text_IO; use Ada.Text_IO;


package body Generic_Data_Integer is

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




function Physical
        (BZERO : in TF; 
        BSCALE : in TF; 
        BLANK : in T; 
        Data  : in T) return TF
is
  D : TF := TF(Data);
begin
--  if (Data = BLANK) return NaN; end if;
--  FIXME See IEEE 748 standard what bit-pattern is NaN and explicitely use that
  return BZERO + BSCALE * D;  
end Physical;




end Generic_Data_Integer;
