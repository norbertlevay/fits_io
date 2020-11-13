
with Header; -- Valued_Key_Record_Arr needed

package Init is

   -- Array_Keys: BZERO BSCALE BUNIT BLANK DATAMIN DATAMAX

   type Access_Rec is record
      BITPIX : Integer;
      A,B : Float;
      Undef_Used : Boolean;
      Undef_Raw  : Float;
      Undef_Phys : Float;
   end record;

   procedure Init_Reads
      (BITPIX    : Integer;
      Array_Keys : in Header.Valued_Key_Record_Arr;
      A          : in Float := 0.0;
      B          : in Float := 1.0;
      Undef_Phys_Valid : in Boolean := False;
      Undef_Phys       : in Float   := 0.0;
      DU_Access   : out Access_Rec);


   procedure Init_Writes
      (BITPIX         : in Integer;
      Undef_Phys_Used : in Boolean;
      Undef_Phys      : in Float;
      A               : in Float := 0.0;
      B               : in Float := 1.0;
      Undef_Raw_Valid : in Boolean := False;
      Undef_Raw       : in Float   := 0.0;
      DU_Access       : out Access_Rec);

   function To_Array_Keys(DU_Access : Access_Rec) return Header.Valued_Key_Record_Arr;


end Init;
