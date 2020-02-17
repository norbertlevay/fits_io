
with Data_Block;
with Data_Funcs; use Data_Funcs;
with Data_Value; use Data_Value;

package body Data_Unit is

 package gen is new Data_Block (T => T); 





procedure Read_Array_Values
   (F : SIO.File_Type;
    Length : in Positive_Count;
    First  : in Positive := 1)
is
   gBlock : gen.Block;
   gen_N  : Positive_Count := Positive_Count(gen.N);--FIXME Block size

   -- calc data array limits
   Length_blocks : constant Positive_Count
            := DU_Block_Index(Positive_Count(First) + Length - 1, T'Size/8);--FIXME Block
   Last_Data_Element_In_Block : constant Positive
            := Offset_In_Block(Positive_Count(First) + Length - 1, gen_N);--FIXME Block

  gValue : T;
  Last_Block_Start : Positive;
 begin

        for I in 1 .. (Length_blocks - 1)
        loop
                gen.Block'Read(SIO.Stream(F),gBlock);
                for K in Positive(First) .. gen.N -- FIXME Block size
                loop
                        gValue := gBlock(K);
                        Element(gValue);
                end loop;
        end loop;

        -- Last Block of InFile

    if(Length_blocks = 1)
    then Last_Block_Start := Positive(First);--FIXME Block size
    else Last_Block_Start := 1;
    end if;   
 
        gen.Block'Read(SIO.Stream(F),gBlock);
        for K in Last_Block_Start .. (Last_Data_Element_In_Block)
        loop
                gValue := gBlock(K);
                Element(gValue);
        end loop;

end Read_Array_Values;





 -- Physical values

package body Physical is




 procedure Read_Valid_Scaled_Values
                 (F : SIO.File_Type;
                 Length : in Positive_Count;
                 BZERO  : in Tout;
                 BSCALE : in Tout;
                 Undef_Val : in Tout;
                 First  : in Positive := 1)
 is
   procedure cbLocArrVal(V : in T)
   is  
    function PhysVal is
    new Valid_Scaled_Value(T, Tout, BZERO, BSCALE, Undef_Val, Is_Valid, "+","*","+");
    Vout : Tout := PhysVal(V);
   begin
    if(Vout = Undef_Val) then Invalid; end if;
    Element_Value(Vout);
   end cbLocArrVal;
   procedure ReadArrVals is new Read_Array_Values(cbLocArrVal);
 begin
   ReadArrVals(F, Length, First);  
 end Read_Valid_Scaled_Values;


 procedure Read_Matched_Valid_Scaled_Values
   (F : SIO.File_Type;
    Length : in Positive_Count;
    BZERO  : in Tout;
    BSCALE : in Tout;
    BLANK  : in T;
    Undef_Val : in Tout;
    First : in Positive := 1)
  is
   procedure cbLocArrVal(V : in T)
   is  
    function PhysVal is
      new Matched_Valid_Scaled_Value(T, Tout, BZERO, BSCALE, BLANK, Undef_Val, Is_Valid, "+","*","+");
    Vout : Tout := PhysVal(V);
   begin
    if(Vout = Undef_Val) then Invalid; end if;
     Element_Value(Vout);
   end cbLocArrVal;
   procedure ReadArrVals is new Read_Array_Values(cbLocArrVal);
 begin
   ReadArrVals(F, Length, First);  
 end Read_Matched_Valid_Scaled_Values;



 procedure Read_Valid_Values
                 (F : SIO.File_Type;
                 Length : in Positive_Count;
                 Undef_Val : in Tout;
         First  : in Positive := 1)
 is
   procedure cbLocArrVal(V : in T)
   is  
    function PhysVal is
    new Valid_Value(T, Tout, Undef_Val, Is_Valid, "+");
    Vout : Tout := PhysVal(V);
   begin
    if(Vout = Undef_Val) then Invalid; end if;
    Element_Value(Vout);
   end cbLocArrVal;
   procedure ReadArrVals is new Read_Array_Values(cbLocArrVal);
 begin
   ReadArrVals(F, Length, First);  
 end Read_Valid_Values;




 procedure Read_Sign_Converted_Integers
                (F : SIO.File_Type;
                Length : in Positive_Count;
                Undef_Val : in Tout;
        First : in Positive := 1)
  is
   procedure cbLocArrVal(V : in T)
   is  
    function PhysVal is new Conv_Signed_Unsigned(T, Tout, Undef_Val, Is_Valid);
    Vout : Tout := PhysVal(V);
   begin
    if(Vout = Undef_Val) then Invalid; end if;
     Element_Value(Vout);
   end cbLocArrVal;
   procedure ReadArrVals is new Read_Array_Values(cbLocArrVal);
 begin
   ReadArrVals(F, Length, First);  
 end Read_Sign_Converted_Integers;



end Physical;

end Data_Unit;
