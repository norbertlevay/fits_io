
with Optional;
with FITS_IO;

with Ada.Text_IO;

package body Init is

   package TIO renames Ada.Text_IO;


   procedure Init_Reads
      (BITPIX  : Integer;
      Array_Keys : in Header.Valued_Key_Record_Arr;
      A           : in Float := 0.0;
      B           : in Float := 1.0;
      Undef_Phys_Valid : in Boolean := False;
      Undef_Phys       : in Float   := 0.0;
      DU_Access   : out Access_Rec)
   is
       -- Tab11 UInt-Int conversion shift
      Aui : Float := 0.0; -- FIXME use pool::Scale_AB(T, Raw_BITPIX,...)
      Ah, Bh : Float; -- A,B from Header BZERO BSCALE
      use FITS_IO.BS_8;
      Undef_Raw_Used : Boolean := False;
      Undef_Raw : Float;
   begin

      DU_Access.BITPIX := BITPIX;

      -- calc [A,B]

      for I in Array_Keys'Range
      loop
         if(Array_Keys(I).Key    = "BZERO   ")
         then
            Ah := Float'Value(FITS_IO.BS70.To_String(Array_Keys(I).Value));
         elsif(Array_Keys(I).Key = "BSCALE  ")
         then
            Bh := Float'Value(FITS_IO.BS70.To_String(Array_Keys(I).Value));
         elsif(Array_Keys(I).Key = "BLANK   ")
         then
            Undef_Raw_Used := True;
            Undef_Raw := Float'Value(FITS_IO.BS70.To_String(Array_Keys(I).Value));
         else
            Ah := 0.0;
            Bh := 1.0;
         end if;
      end loop;

      DU_Access.A := A + Ah + Aui;
      DU_Access.B := B * Bh;


      -- calc Undef

      DU_Access.Undef_Used := Undef_Raw_Used;

      if(DU_Access.Undef_Used)
      then

         DU_Access.Undef_Raw := Undef_Raw;

         if(Undef_Phys_Valid)
         then
            DU_Access.Undef_Phys := Undef_Phys;
         else
            DU_Access.Undef_Phys := DU_Access.A + DU_Access.B * DU_Access.Undef_Raw;
         end if;

      end if;


   end Init_Reads;







   procedure Init_Writes
      (BITPIX         : in Integer;
      Undef_Phys_Used : in Boolean;
      Undef_Phys      : in Float;
      A               : in Float := 0.0;
      B               : in Float := 1.0;
      Undef_Raw_Valid : in Boolean := False;
      Undef_Raw       : in Float   := 0.0;
      DU_Access       : out Access_Rec)
   is
      -- Tab11 UInt-Int conversion shift
      Aui : Float := 0.0; -- FIXME use pool::Scale_AB(T, Raw_BITPIX,...)
   begin

      DU_Access.BITPIX := BITPIX;

      -- calc [A,B]

      DU_Access.A := A + Aui;
      DU_Access.B := B;


      -- calc Undef

      DU_Access.Undef_Used := Undef_Phys_Used;

      if(DU_Access.Undef_Used)
      then

         DU_Access.Undef_Phys := Undef_Phys;

         if(Undef_Raw_Valid)
         then
            DU_Access.Undef_Raw := Undef_Raw;
         else
            DU_Access.Undef_Raw := (DU_Access.Undef_Phys - DU_Access.A) / DU_Access.B;
         end if;

      end if;

   end Init_Writes;



   -- convert Access_Rec - Reserved.Array_Keys cards

   function To_Array_Keys(DU_Access : Access_Rec) return Header.Valued_Key_Record_Arr
   is
      Ncards : Natural := 0;
      KeysBuffer : Header.Valued_Key_Record_Arr(1..3);
      use FITS_IO.BS_8;
      use FITS_IO.BS70;
   begin

      if(DU_Access.A /= 0.0)
      then
         Ncards := Ncards + 1;
         KeysBuffer(Ncards).Key   := 1 * "BZERO   ";
         KeysBuffer(Ncards).Value := 1 * Float'Image(DU_Access.A);
      end if;

      if(DU_Access.B /= 1.0)
      then
         Ncards := Ncards + 1;
         KeysBuffer(Ncards).Key   := 1 * "BSCALE  ";
         KeysBuffer(Ncards).Value := 1 * Float'Image(DU_Access.B);
      end if;

      if(DU_Access.Undef_Used)
      then
         Ncards := Ncards + 1;
         KeysBuffer(Ncards).Key   := 1 * "BLANK   ";
         KeysBuffer(Ncards).Value := 1 * Float'Image(DU_Access.Undef_Raw);
      end if;

      declare
         ArrKeys : Header.Valued_Key_Record_Arr := KeysBuffer(1..Ncards);
      begin
         return ArrKeys;
      end;
   end To_Array_Keys;



   -- utils
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




end Init;
