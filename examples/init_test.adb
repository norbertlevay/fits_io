
with Ada.Text_IO;

with FITS_IO;
with Header;
with Init;

procedure init_test
is

   package TIO renames Ada.Text_IO;
   use FITS_IO.BS_8;
   use FITS_IO.BS70;

      BITPIX          :  Integer  := 32;
      Undef_Phys_Used :  Boolean  := False;
      Undef_Phys      :  Float    := 0.0;
--      A               :  Float := 0.0;
--      B               :  Float := 1.0;
--      Undef_Raw_Valid :  Boolean := False;
--      Undef_Raw       :  Float   := 0.0;
      DUAccess : Init.Access_Rec;
      ArrKeys  : Header.Valued_Key_Record_Arr := (
         (1*"BZERO   ", 1*"5.0"),
         (1*"BSCALE  ", 1*"6.0"),
         (1*"BLANK   ", 1*"32768")
         );

   procedure Put_Access_Rec(AccRec : Init.Access_Rec)
   is
     sBITPIX : String := Integer'Image(AccRec.BITPIX);
     sA : String := Float'Image(AccRec.A);
     sB : String := Float'Image(AccRec.B);
     sUndef_Used : String := Boolean'Image(AccRec.Undef_Used);
     sUndef_Raw  : String := Float'Image(AccRec.Undef_Raw);
     sUndef_Phys : String := Float'Image(AccRec.Undef_Phys);
  begin

   TIO.Put_Line("BITPIX = " & sBITPIX);
   TIO.Put_Line("[A,B]  = " & sA & " " & sB);
   if(AccRec.Undef_Used)
   then
      TIO.Put_Line("Undef_Raw  = " & sUndef_Raw);
      TIO.Put_Line("Undef_Phys = " & sUndef_Phys);
   end if;

  end Put_Access_Rec;

  procedure Put_Array_Keys(Keys : Header.Valued_Key_Record_Arr)
  is
     use FITS_IO.BS_8;
     use FITS_IO.BS70;
  begin
     for I in Keys'Range
      loop
         TIO.Put_Line(To_String(Keys(I).Key) & " " & To_String(Keys(I).Value));
      end loop;
  end Put_Array_Keys;



begin

   Put_Array_Keys(ArrKeys);
   TIO.New_LIne;

   Init.Init_Reads
      (BITPIX => 32,
      Array_Keys => ArrKeys,--: in Header.Valued_Key_Record_Arr;
      --A           : in Float := 0.0;
      --B           : in Float := 1.0;
      Undef_Phys_Valid => True,-- : in Boolean := False;
      Undef_Phys => 6.0,-- : in Float   := 0.0;
      DU_Access => DUAccess);--   : out Access_Rec);

   TIO.Put_Line("Init_Reads:");
   Put_Access_Rec(DUAccess);



   Init.Init_Writes
      (BITPIX => 32,
      Undef_Phys_Used => True,
      Undef_Phys => 2.0,
--      A => 11.0,--              : in Float := 0.0;
--      B => 31.0,--              : in Float := 1.0;
--      Undef_Raw_Valid : in Boolean := False;
--      Undef_Raw       : in Float   := 0.0;
      DU_Access  => DUAccess);

   TIO.New_Line;
   TIO.Put_Line("Init_Writes:");
   Put_Access_Rec(DUAccess);

   TIO.New_Line;
   Put_Array_Keys(Init.To_Array_Keys(DUAccess));

end init_test;
