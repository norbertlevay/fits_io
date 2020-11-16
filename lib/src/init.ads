

-- FIXME missing support for unsigned Int (Tab11)


with Header; -- Valued_Key_Record_Arr needed

package Init is

   type DU_Type is (
       Int8, UInt16, UInt32, UInt64,
      UInt8,  Int16,  Int32,  Int64,
      F32, F64);

   -- implements signed-unsigned conversions for integer types as of Tab11
   procedure DU_Type_To_BITPIX
      (DUType : DU_Type;
      BITPIX  : out Integer;
      Aui     : out Float);

   -- Array_Keys: BZERO BSCALE BUNIT BLANK DATAMIN DATAMAX

   -- Access_Rec represents Array_Keys: BZERO BSCALE BLANK
   -- BZERO BSCALE are type Float
   -- BLANK is type <BITPIX>

   type Access_Rec is record
      BITPIX : Integer;
      A,B : Float;
      Undef_Used : Boolean;
      Undef_Raw  : Float;
      Undef_Phys : Float;
   end record;

   -- triple [DU_Type, A,B] allows to view any of DU_Type as T
   procedure Init_Reads
      (DUType    : in DU_Type;
      Array_Keys : in Header.Valued_Key_Record_Arr;
      A          : in Float := 0.0;
      B          : in Float := 1.0;
      Undef_Phys_Valid : in Boolean := False;
      Undef_Phys       : in Float   := 0.0;
      DU_Access   : out Access_Rec);

   -- triple [DU_Type, A,B] allows to store type T to any of DU_Types
   procedure Init_Writes
      (DUType    : in DU_Type;
      Undef_Phys_Used : in Boolean;
      Undef_Phys      : in Float;
      A               : in Float := 0.0;
      B               : in Float := 1.0;
      Undef_Raw_Valid : in Boolean := False;
      Undef_Raw       : in Float   := 0.0;
      DU_Access       : out Access_Rec);

   function To_Array_Keys(DU_Access : Access_Rec) return Header.Valued_Key_Record_Arr;



   -- utils
   procedure Put_Access_Rec(AccRec : Init.Access_Rec);
   procedure Put_Array_Keys(Keys : Header.Valued_Key_Record_Arr);
end Init;
