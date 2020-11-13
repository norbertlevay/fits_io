
with Ada.Text_IO;

with Header;
with Init;

procedure init_test
is

   package TIO renames Ada.Text_IO;

      BITPIX          :  Integer  := 32;
      Undef_Phys_Used :  Boolean  := False;
      Undef_Phys      :  Float    := 0.0;
--      A               :  Float := 0.0;
--      B               :  Float := 1.0;
--      Undef_Raw_Valid :  Boolean := False;
--      Undef_Raw       :  Float   := 0.0;
      ArrKeys  : Header.Valued_Key_Record_Arr(1..5);
      DUAccess : Init.Access_Rec;

   procedure Put_Access_Rec(AccRec : Init.Access_Rec)
   is
     sA : String := Float'Image(AccRec.A);
     sB : String := Float'Image(AccRec.B);
     sUndef_Valid : String := Boolean'Image(AccRec.Undef_Valid);
     sUndef_Raw   : String := Float'Image(AccRec.Undef_Raw);
     sUndef_Phys  : String := Float'Image(AccRec.Undef_Phys);
  begin

   TIO.Put_Line("[A,B] = " & sA & " " & sB);
   if(AccRec.Undef_Valid)
   then
      TIO.Put_Line("Undef_Raw  = " & sUndef_Raw);
      TIO.Put_Line("Undef_Phys = " & sUndef_Phys);
   end if;

  end Put_Access_Rec;


begin

   Init.Init_Writes
      (BITPIX => 32,
      Undef_Phys_Used => False,
      Undef_Phys => 0.0,
--      A               : in Float := 0.0;
--      B               : in Float := 1.0;
--      Undef_Raw_Valid : in Boolean := False;
--      Undef_Raw       : in Float   := 0.0;
      Array_Keys => ArrKeys,
      DU_Access  => DUAccess);


   Put_Access_Rec(DUAccess);


end init_test;
