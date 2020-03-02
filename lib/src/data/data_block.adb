
with Ada.Unchecked_Conversion;
with System; use System;-- Bit_Order neede for Endianness

with Data_Funcs; use Data_Funcs;

with Interfaces; -- Byte needed (as Unsigned_8)


package body Data_Block is

    -- this func exists onyl for generalization: if Storage_Size is not 8 bit
    -- (e.g. Stream_Element is not 8 bit) this fucntion has to perform
    -- reading out the data in any case
    -- if SE'Size = 8bit it is trivial (and inlined)
    function Get_Value(Blk: Block; Index : in Positive) return T
    is
    begin
        return Blk(Index);
    end Get_Value;
    pragma Inline(Get_Value);


    procedure Set_Value(Blk: in out Block; Index : in Positive; Value : in T)
    is
    begin
        Blk(Index) := Value;
    end Set_Value;
    pragma Inline(Get_Value);


  -- Endianness

   procedure Revert_Bytes( Data : in out T )
   is 
     Size_Bytes : Positive := T'Size / Interfaces.Unsigned_8'Size;
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
    -- FIXME Stream_Element'Size might not be 8 (then 2880 is not correct)
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

end Data_Block;
