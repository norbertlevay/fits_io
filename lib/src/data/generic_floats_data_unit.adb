
with FITS;
with Generic_Data_Block;
with Data_Funcs;  use Data_Funcs;
with Strict; use Strict; -- Positive_Arr needed
with Keyword_Record; use Keyword_Record; -- FNatural needed

with Generic_Data_Value; use Generic_Data_Value;

package body Generic_FLoats_Data_Unit is


 procedure Read_Checked_Floats
                (F : SIO.File_Type;
                Length : in Positive;
                BZERO  : in Tout;
                BSCALE : in Tout)
  is


   procedure LocArrVal(V : in Tin)
   is 
   --function PhysVal is new Checked_Physical_Float(Tin, Tout, BZERO, BSCALE, "+","*","+");
    function PhysVal is new Checked_Physical_Float(Tin, Tout, BZERO, BSCALE);
   begin
     Element_Value(PhysVal(V));
     -- NOTE inplicit conversion Tin->Tout: down-conversions 
     -- like Float_64 -> Float_32 suffer loss of precision
   exception 
     when Except_ID : Generic_Data_Value.Undefined_Value => Undefined_Value;
   end LocArrVal;


   procedure ReadArrVals is new FDU.Read_Array_Values(LocArrVal);


 begin
   ReadArrVals(F, Length);  
 end Read_Checked_Floats;











end Generic_Floats_Data_Unit;
