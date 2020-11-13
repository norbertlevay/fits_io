

package body Init is

   procedure Init_Writes
      (BITPIX         : in Integer;
      Undef_Phys_Used : in Boolean;
      Undef_Phys      : in Float;
      A               : in Float := 0.0;
      B               : in Float := 1.0;
      Undef_Raw_Valid : in Boolean := False;
      Undef_Raw       : in Float   := 0.0;
      Array_Keys      : out Header.Valued_Key_Record_Arr;
      DU_Access       : out Access_Rec)
   is
   begin






      -- set results
      DU_Access.A := 0.0;
      DU_Access.B := 1.0;
      DU_Access.Undef_Valid := False;
      DU_Access.Undef_Raw   := 0.0;
      DU_Access.Undef_Phys  := 0.0;

   end Init_Writes;


end Init;
