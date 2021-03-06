
with Ada.Unchecked_Conversion;
with Interfaces;
with System; use System;

with Ada.Streams;

with FITS; -- Byte-type needed

-- DBG only with Ada.Text_IO; use Ada.Text_IO;


package body Generic_Data_Types is

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





 procedure T_Read_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : out Block )
   is  
        type SBlock is array (1 .. 2880) of Ada.Streams.Stream_Element;
        function S_To_D is
                new Ada.Unchecked_Conversion(Source => SBlock, Target => Block);
        sb : SBlock;
   begin
      --Put_Line("GenFloat::T_Read_BigEndian");

     SBlock'Read(S,sb);
     Data := S_To_D(sb);

     if System.Default_Bit_Order = System.LOW_ORDER_FIRST
     then
        for I in Block'Range
        loop
           Revert_Bytes(Data(I));
        end loop;
     end if;

   end T_Read_BigEndian;



  procedure T_Write_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : in Block ) 
   is  
        DD : Block := Data;-- Revert_Bytes is in out This func is in only

        type SBlock is array (1 .. 2880) of Ada.Streams.Stream_Element;
        function D_To_S is
                new Ada.Unchecked_Conversion(Source => Block, Target => SBlock);
   begin
      --Put_Line("GenFloat::T_Write_BigEndian");

     if System.Default_Bit_Order = System.LOW_ORDER_FIRST
     then
        for I in Block'Range
        loop
               Revert_Bytes(DD(I));
        end loop;
     end if;

     declare
        ob : SBlock := D_To_S(DD);
     begin
        SBlock'Write(S,ob);
     end;

   end T_Write_BigEndian;

end Generic_Data_Types;
