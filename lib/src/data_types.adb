with Ada.Unchecked_Conversion;
with System; use  System;

with FITS; -- Byte-type needed

-- DBG only with Ada.Text_IO; use Ada.Text_IO;


package body Data_Types is

  generic
     type Data_Type is private;
   procedure Revert_Bytes( Data : in out Data_Type );

   procedure Revert_Bytes( Data : in out Data_Type )
   is  
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



-- Apply to FITS data types (BITPIX)




-- Integer_32

   procedure Int32_Read_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : out Integer_32 )
   is  
     procedure I32_Revert_Bytes is
       new Revert_Bytes( Data_Type => Integer_32 );
   begin

     Interfaces.Integer_32'Read(S,Interfaces.Integer_32(Data));

     if System.Default_Bit_Order = System.LOW_ORDER_FIRST
     then
       I32_Revert_Bytes(Data);
     end if;

   end Int32_Read_BigEndian;

   procedure Int32_Write_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : in Integer_32 )
   is
     procedure I32_Revert_Bytes is
       new Revert_Bytes( Data_Type => Integer_32 );
     DD : Integer_32 := Integer_32(Data);
   begin

     if System.Default_Bit_Order = System.LOW_ORDER_FIRST
     then
       I32_Revert_Bytes(DD);
     end if;

     Interfaces.Integer_32'Write(S,Interfaces.Integer_32(DD));

   end Int32_Write_BigEndian;




-- IEEE FLOAT_32

   procedure Float32_Read_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : out Float_32 )
   is  
     procedure F32_Revert_Bytes is
       new Revert_Bytes( Data_Type => Float_32 );
   begin

     Interfaces.IEEE_Float_32'Read(S,Interfaces.IEEE_Float_32(Data));

     if System.Default_Bit_Order = System.LOW_ORDER_FIRST
     then
       F32_Revert_Bytes(Data);
     end if;

   end Float32_Read_BigEndian;

   procedure Float32_Write_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : in Float_32 )
   is
     procedure F32_Revert_Bytes is
       new Revert_Bytes( Data_Type => Float_32 );
     DD : Float_32 := Float_32(Data);
   begin

     if System.Default_Bit_Order = System.LOW_ORDER_FIRST
     then
       F32_Revert_Bytes(DD);
     end if;

     Interfaces.IEEE_Float_32'Write(S,Interfaces.IEEE_Float_32(DD));

   end Float32_Write_BigEndian;





end Data_Types;
