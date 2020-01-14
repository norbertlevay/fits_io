
with Ada.Unchecked_Conversion;
with Interfaces;

with FITS; -- Byte-type needed

-- DBG only with Ada.Text_IO; use Ada.Text_IO;


package body Generic_Data_Types is

   procedure Revert_Bytes( Data : in out Data_Type )
   is 
	-- FIXME review: is FITS.Bytes needed here ? prefer Interfaces.Unsigned_8 ?
     Size_Bytes : Positive := Data_Type'Size / FITS.Byte'Size;
     type Arr4xU8 is array (1..Size_Bytes) of Interfaces.Unsigned_8;

     function Data_To_Arr is
       new Ada.Unchecked_Conversion(Source => Data_Type, Target => Arr4xU8);
     function Arr_To_Data is
       new Ada.Unchecked_Conversion(Source => Arr4xU8, Target => Data_Type);

     Arr  : Arr4xU8 := Data_To_Arr(Data);
     ArrO : Arr4xU8;
   begin

     for I in Arr'Range
     loop
       ArrO(I) := Arr(1 + Size_Bytes - I); 
     end loop;

     Data := Arr_To_Data(ArrO);

   end Revert_Bytes;



  -- Physical-Raw value conversions

function Physical_Value_From_Int
	(BZERO : in TF; 
	BSCALE : in TF; 
	BLANK : in TD;
	Data  : in TD) return TF
is
  D : TF := TF(Data);
begin
--  if (Data = BLANK) return NaN; end if;
--  See IEEE 748 standard what bit-pattern is NaN and explicitely use that
  return BZERO + BSCALE * D; 
end Physical_Value_From_Int;


function Physical_Value_From_Float(BZERO : in TFp; BSCALE : in TFp; Data : in TFd) return TFd
is
begin
  return TFd(BZERO) + TFd(BSCALE) * Data;
end Physical_Value_From_Float;




end Generic_Data_Types;
