
with Ada.Unchecked_Conversion;
with System; use  System;
-- DBG only with Ada.Text_IO; use Ada.Text_IO;

--with FITS; -- Byte-type needed
with Generic_Data_Types;




package body Data_Types is


-- Integer_32

   procedure Int32_Read_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : out Integer_32 )
   is  
   begin

     Interfaces.Integer_32'Read(S,Interfaces.Integer_32(Data));

     if System.Default_Bit_Order = System.LOW_ORDER_FIRST
     then
       Int32.Revert_Bytes(Data);
     end if;

   end Int32_Read_BigEndian;

   procedure Int32_Write_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : in Integer_32 )
   is
     DD : Integer_32 := Integer_32(Data);
   begin

     if System.Default_Bit_Order = System.LOW_ORDER_FIRST
     then
       Int32.Revert_Bytes(DD);
     end if;

     Interfaces.Integer_32'Write(S,Interfaces.Integer_32(DD));

   end Int32_Write_BigEndian;




-- IEEE FLOAT_32

   procedure Float32_Read_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : out Float_32 )
   is  
   begin

     Interfaces.IEEE_Float_32'Read(S,Interfaces.IEEE_Float_32(Data));

     if System.Default_Bit_Order = System.LOW_ORDER_FIRST
     then
       Float32.Revert_Bytes(Data);
     end if;

   end Float32_Read_BigEndian;

   procedure Float32_Write_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : in Float_32 )
   is
     DD : Float_32 := Float_32(Data);
   begin

     if System.Default_Bit_Order = System.LOW_ORDER_FIRST
     then
       Float32.Revert_Bytes(DD);
     end if;

     Interfaces.IEEE_Float_32'Write(S,Interfaces.IEEE_Float_32(DD));

   end Float32_Write_BigEndian;



end Data_Types;
