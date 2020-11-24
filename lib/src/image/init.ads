

with Header; -- Valued_Key_Record_Arr needed

with FITS_IO; use FITS_IO;

package Init is

   type DU_Type_MOVED_TO_PARENT is (
       Int8, UInt16, UInt32, UInt64,
      UInt8,  Int16,  Int32,  Int64,
      F32, F64);

   subtype DU_Int_Type is DU_Type range Int8 .. Int64;


   -- implements signed-unsigned conversions for integer types as of Tab11
   procedure DU_Type_To_BITPIX
      (DUType : DU_Type;
      BITPIX  : out Integer;
      Aui     : out Float);

   -- Array_Keys: BZERO BSCALE BUNIT BLANK DATAMIN DATAMAX

   procedure Linear_Scale
      (DUIntType : in DU_Int_Type;
      DATAMIN, DATAMAX : in Float; -- DATAMAX /= DATAMIN
      A : out Float; B : out Float);

   -- FIXME Linear_Scale() includes the Tab11 A-shift; Init_Reads/Writes should not shift again !!
   -- ?? remove from inside Init_Reads/Writes the Tab11-shift and provide - as Linear_Scale -
   -- as separate API element ?? Then Init_Reads/Writes depends on BITPIX again -> remove BITPIX
   -- from Init_Reads and Access_Rec ??



   -- Access_Rec represents Array_Keys: BZERO BSCALE BLANK
   -- BZERO BSCALE are type Float
   -- BLANK is type <BITPIX>

   type Access_Rec_MOVED is record
      BITPIX : Integer;
      A,B : Float;
      Undef_Used : Boolean;
      Undef_Raw  : Float;
      Undef_Phys : Float;
   end record;


   -- triple [DU_Type, A,B] allows to view any of DU_Type as T
   procedure Init_Reads
      (BITPIX  : in Integer;
      Image_Cards : in String_80_Array;
      A          : in Float := 0.0;
      B          : in Float := 1.0;
      Undef_Phys_Valid : in Boolean := False;
      Undef_Phys       : in Float   := 0.0;
      DU_Access   : out Access_Rec);

   -- triple [DU_Type, A,B] allows to store type T to any of DU_Types
   procedure Init_Writes
      (BITPIX : in Integer;
      Image_Cards : in String_80_Array;
      Undef_Phys_Used : in Boolean;
      Undef_Phys      : in Float;
      A               : in Float := 0.0;
      B               : in Float := 1.0;
      DU_Access       : out Access_Rec);

   function To_Array_Keys(DU_Access : Access_Rec) return Header.Valued_Key_Record_Arr;



   -- utils
   procedure Put_Access_Rec(AccRec : Access_Rec; Prefix : String := "");
   procedure Put_Array_Keys(Keys : Header.Valued_Key_Record_Arr; Prefix : String := "");
end Init;
