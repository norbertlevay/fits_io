
with FITS;
with Generic_Data_Block;
with Data_Funcs;  use Data_Funcs;
with Strict; use Strict; -- Positive_Arr needed
with Keyword_Record; use Keyword_Record; -- FNatural needed

with Generic_Data_Value; use Generic_Data_Value;

package body Generic_Data_Unit is

 package gen is new Generic_Data_Block (T => T); 





procedure Read_Array_Values(F : SIO.File_Type; DUSize : in Positive)
is
   -- define Data Block
   gBlock  : gen.Block;

   -- calc data array limits
   DUSize_blocks : constant Positive := DU_Block_Index(Positive(DUSize),T'Size/8);
   Last_Data_Element_In_Block : constant Positive :=  
                                        Offset_In_Block(Positive(DUSize), gen.N);
  -- local vars
  gValue : T;
 begin

        for I in 1 .. (DUSize_Blocks - 1)
        loop
                gen.Block'Read(SIO.Stream(F),gBlock);
                for K in 1 .. gen.N
                loop
                        gValue := gBlock(K);
                        Element(gValue);
                end loop;
        end loop;

        -- Last Block of InFile
    
        gen.Block'Read(SIO.Stream(F),gBlock);
        for K in 1 .. (Last_Data_Element_In_Block)
        loop
                gValue := gBlock(K);
                Element(gValue);
        end loop;

end Read_Array_Values;





 -- Physical values

package body Physical is

 procedure Read_Values
                 (F : SIO.File_Type;
                 DUSize : in Positive;
                 BZERO  : in Tout;
                 BSCALE : in Tout)
 is
   procedure LocArrVal(V : in T)
   is  
    function PhysVal is new Physical_Value(T, Tout, BZERO, BSCALE, "+","*","+");
   begin
    Element_Value(PhysVal(V));
   end LocArrVal;
   procedure ReadArrVals is new Read_Array_Values(LocArrVal);
 begin
   ReadArrVals(F, DUSize);  
 end Read_Values;


 generic
  with function  Is_Undefined(V : in T) return Boolean;
  with procedure Undefined_Value;
  with procedure Element_Value(V : in Tout);
 procedure OFFRead_Checked_Values
               (F : SIO.File_Type;
               DUSize : in Positive;
               BZERO  : in Tout;
               BSCALE : in Tout);
 procedure OFFRead_Checked_Values
                (F : SIO.File_Type;
                DUSize : in Positive;
                BZERO  : in Tout;
                BSCALE : in Tout)
 is
   procedure LocArrVal(V : in T)
   is  
    function PhysVal is new OFFChecked_Physical_Value(T, Tout, BZERO, BSCALE, Is_Undefined, "+","*","+");
   begin
    Element_Value(PhysVal(V));
    exception 
      when Except_ID : Generic_Data_Value.Undefined_Value => Undefined_Value;
   end LocArrVal;
   procedure ReadArrVals is new Read_Array_Values(LocArrVal);
 begin
   ReadArrVals(F, DUSize);  
 end OFFRead_Checked_Values;



 procedure Read_Checked_Integers
                (F : SIO.File_Type;
                DUSize : in Positive;
                BZERO  : in Tout;
                BSCALE : in Tout;
                BLANK  : in T)
  is
   procedure LocArrVal(V : in T)
   is  
    function PhysVal is new Checked_Physical_Integer(T, Tout, BZERO, BSCALE, BLANK, "+","*","+");
   begin
     Element_Value(PhysVal(V));
    exception 
     when Except_ID : Generic_Data_Value.Undefined_Value => Undefined_Value;
   end LocArrVal;
   procedure ReadArrVals is new Read_Array_Values(LocArrVal);
 begin
   ReadArrVals(F, DUSize);  
 end Read_Checked_Integers;


end Physical;



package body Physical_Float is

 procedure Read_Checked_Floats
                (F : SIO.File_Type;
                DUSize : in Positive;
                BZERO  : in Tout;
                BSCALE : in Tout)
  is
   procedure LocArrVal(V : in T)
   is  
    function PhysVal is new Checked_Physical_Float(Tout, Tout, BZERO, BSCALE, "+","*","+");
   begin
     Element_Value(PhysVal(+(V))); -- FIXME note the conversion +()
     -- func has in name _Floats so will be used in instances where T is Float
     -- FIXME still try search for better sulution without the +(V) conversion
   exception 
     when Except_ID : Generic_Data_Value.Undefined_Value => Undefined_Value;
   end LocArrVal;
   procedure ReadArrVals is new Read_Array_Values(LocArrVal);
 begin
   ReadArrVals(F, DUSize);  
 end Read_Checked_Floats;





end Physical_Float;






end Generic_Data_Unit;
