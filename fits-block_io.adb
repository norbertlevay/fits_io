
with Ada.Unchecked_Conversion;
with System;
use  System;

package body FITS.Block_IO is

package body DataBlock is

   procedure Revert_Bytes( Data : in out Data_Type )
   is
     Size_Bytes : Positive := Data'Size / 8;-- [FITS] defines byte as 8 bit
                                 -- FIXME don't use 8 literal
     type Arr4xU8 is array (1..Size_Bytes) of Interfaces.Unsigned_8;

     function Data_To_Arr is
       new Ada.Unchecked_Conversion(Source => Data_Type, Target => Arr4xU8);
     function Arr_To_Data is
       new Ada.Unchecked_Conversion(Source => Arr4xU8, Target => Data_Type);

     Arr : Arr4xU8 := Data_To_Arr(Data);
   begin

     for I in Arr'Range
     loop
       Arr(I) := Arr(1 + Size_Bytes - I);
     end loop;

   end Revert_Bytes;

   procedure Write_BigEndian
    		(S    : access Ada.Streams.Root_Stream_Type'Class;
             	 Data : in Data_Type )
   is
   begin

     if System.Default_Bit_Order = System.LOW_ORDER_FIRST
     then
       null;
-- FIXME use generic package or make 1 functions Revert_Bytes(Data);
     end if;

     Data_Type'Write(S,Data);

   end Write_BigEndian;

end DataBlock;

    procedure Write_BigEndian_Float32
     		(S    : access Ada.Streams.Root_Stream_Type'Class;
              	 Data : in Float_32 )
    is
      package Float32 is new DataBlock(Float_32);
    begin
      Float32.Write_BigEndian(S,Data);
    end Write_BigEndian_Float32;
end FITS.Block_IO;

