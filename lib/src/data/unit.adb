
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Data_Value; use Data_Value;
with Data_Funcs; use Data_Funcs;
with Data_Block;

package body Unit is




procedure Read_Array
   (F : SIO.File_Type;
    Values : out T_Arr;
    First  : in Positive := 1)
is
 package SIO renames Ada.Streams.Stream_IO;
 use SIO;


  package gen is new Data_Block (T => T);

   gBlock : gen.Block;
   gen_N  : Positive_Count := Positive_Count(gen.N);--FIXME Block size

   Length : Positive_Count := Values'Length;

   -- calc data array limits
   Length_blocks : constant Positive_Count
            := DU_Block_Index(Positive_Count(First) + Length - 1, T'Size/8);--FIXME Block
   Last_Data_Element_In_Block : constant Positive
            := Offset_In_Block(Positive_Count(First) + Length - 1, gen_N);--FIXME Block

  gValue : T;
  Last_Block_Start : Positive;
  LocFirst : Positive := First;
  DUIndex : Positive_Count := 1;
begin

        for I in 1 .. (Length_blocks - 1)
        loop
                gen.Block'Read(SIO.Stream(F),gBlock);
                for K in LocFirst .. gen.N -- FIXME Block size
                loop
                        gValue := gen.Get_Value(gBlock, K);
                        Values(DUIndex) := gValue;
                        DUIndex := DUIndex + 1;
                end loop;
                LocFirst := 1;
        end loop;

        -- Last Block of InFile

    if(Length_blocks = 1)
    then Last_Block_Start := First;--FIXME Block size
    else Last_Block_Start := 1;
    end if;

        gen.Block'Read(SIO.Stream(F),gBlock);
        for K in Last_Block_Start .. (Last_Data_Element_In_Block)
        loop
                gValue := gen.Get_Value(gBlock, K);
                Values(DUIndex) := gValue;
        end loop;

end Read_Array;





procedure Write_Array
   (F : SIO.File_Type;
    Values : in T_Arr;
    First  : in Positive := 1)
is
begin
    null;
end Write_Array;





 function Scale(Vf : in Tf) return Tm
 is
 begin
    return +( BZERO + BSCALE * (+Vf) );
 end Scale;





 function Scale_Float(Vf : in Tf) return Tm
 is
 begin
 if(Vf'Valid)
 then
    return Undef;
 else
    return +( BZERO + BSCALE * (+Vf) );
 end if;
 end Scale_Float;



end Unit;

