

with Generic_Data_Block;
with Data_Funcs;  use Data_Funcs;
with Strict; use Strict; -- Positive_Arr needed
with Keyword_Record; use Keyword_Record; -- FNatural needed

package body Generic_Data_Unit is

  package gen is new Generic_Data_Block (T => T); 



 procedure MinMax(F : SIO.File_Type;
	     	DUSize : in Positive; 
	     	Min : out TF; Max : out TF) 
 is


   -- define Data Block
   gBlock  : gen.Block;
   function T_Physical_Value is new gen.Physical_Value(TF => TF, To_TF => Conv_TF);

   -- calc data array limits
   DUSize_blocks : constant Positive := DU_Block_Index(Positive(DUSize),T'Size/8);
   Last_Data_Element_In_Block : constant Positive :=  
                                        Offset_In_Block(Positive(DUSize), gen.N);
  -- local vars
  gValue : T;
  lMin : TF := B_Min;
  lMax : TF := B_Max;
  pVal : TF; 
 begin
        for I in 1 .. (DUSize_Blocks - 1)
        loop
                gen.Block'Read(SIO.Stream(F),gBlock);
                for K in 1 .. gen.N
                loop
                        gValue := gBlock(K);
                        pVal := T_Physical_Value(TF(BZERO),TF(BSCALE),gValue);
                        if(pVal < lMin) then lMin := pVal; end if;
                        if(pVal > lMax) then lMax := pVal; end if;
                end loop;
        end loop;

        -- Last Block of InFile
    
        gen.Block'Read(SIO.Stream(F),gBlock);
        for K in 1 .. (Last_Data_Element_In_Block)
        loop
                gValue := gBlock(K);
                pVal := T_Physical_Value(TF(BZERO),TF(BSCALE),gValue);
                if(pVal < lMin) then lMin := pVal; end if;
                if(pVal > lMax) then lMax := pVal; end if;
        end loop;

        Min := lMin;
        Max := lMax;

 end MinMax;



end Generic_Data_Unit;
