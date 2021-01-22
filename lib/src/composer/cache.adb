
with Ada.Text_IO;


package body Cache is

   package TIO renames Ada.Text_IO;

   -- Access_Rec

   procedure Put_Access_Rec(AccRec : Access_Rec; Prefix : String := "")
   is
      sBITPIX : String := Integer'Image(AccRec.BITPIX);
      sA : String := Float'Image(AccRec.A);
      sB : String := Float'Image(AccRec.B);
      sUndef_Used : String := Boolean'Image(AccRec.Undef_Used);
      sUndef_Raw  : String := Float'Image(AccRec.Undef_Raw);
      sUndef_Phys : String := Float'Image(AccRec.Undef_Phys);
   begin

      TIO.Put_Line(Prefix & "BITPIX = " & sBITPIX);
      TIO.Put_Line(Prefix & "[A,B]  = " & sA & " " & sB);
      if(AccRec.Undef_Used)
      then
         TIO.Put_Line(Prefix & "Undef_Raw  = " & sUndef_Raw);
         TIO.Put_Line(Prefix & "Undef_Phys = " & sUndef_Phys);
      end if;

   end Put_Access_Rec;


   procedure Parse_Image_Cards
     (Cache : in out Cache_Rec;
     Cards : in String_80_Array)
   is  
   begin
      -- init
      Cache.Ah := 0.0; Cache.Bh:= 1.0;
      -- overwrite inited if exists
      for I in Cards'Range
      loop
         if(Cards(I)(1 .. 5) = "BZERO")
         then
            Cache.Ah := Float'Value(Cards(I)(11 .. 30));
         elsif(Cards(I)(1 .. 6) = "BSCALE")
         then
            Cache.Bh := Float'Value(Cards(I)(11 .. 30));
         elsif(Cards(I)(1 .. 5) = "BLANK")
         then
            Cache.Raw_Undef_Valid := True;
            Cache.Raw_Undef_Value := Float'Value(Cards(I)(11 .. 30));
         end if;
      end loop;
   end Parse_Image_Cards;





   -- Cache

   procedure Load_BITPIX_And_Scaling_AB(Scaling : in out Access_Rec; Cache : Cache_Rec)
   is  
   begin
      -- A,B interpreted always in Read-direction: Phys = A + B * Raw
      -- inverted values [-A/B , 1/B] used in Write for Undef calc and data scaling
      -- direct values [A,B] used in Read
      Scaling.A := Cache.Ah + Cache.Au + Cache.Aui;
      Scaling.B := Cache.Bh * Cache.Bu;

      Scaling.BITPIX := Cache.BITPIX;
   end Load_BITPIX_And_Scaling_AB;


   procedure Load_Undef_Vals_At_Write(Scaling : in out Access_Rec; Cache : in out Cache_Rec)
   is  
   begin
      -- Write: Phys -> Raw
      Scaling.Undef_Used := Cache.Physical_Undef_Valid;
      if(Scaling.Undef_Used)
      then
         Scaling.Undef_Phys := Cache.Physical_Undef_Value;
         if(Cache.Raw_Undef_Valid)
         then
            Scaling.Undef_Raw := Cache.Raw_Undef_Value;
         else
            -- FIXME Undef calc is type dependent: if Float Undef is NaN
            -- can be calc'd by Scaling only for (U)Int's
            Scaling.Undef_Raw :=
               (Cache.Physical_Undef_Value - Scaling.A) / Scaling.B;
               Cache.Raw_Undef_Valid := True;
         end if;
      end if;
   end Load_Undef_Vals_At_Write;


   procedure Load_Undef_Vals_At_Read(Scaling : in out Access_Rec; Cache : in out Cache_Rec)
   is  
   begin
      -- Read: Raw -> Phys
      Scaling.Undef_Used := Cache.Raw_Undef_Valid;
      if(Scaling.Undef_Used)
      then
         Scaling.Undef_Raw := Cache.Raw_Undef_Value;
         if(Cache.Physical_Undef_Valid)
         then
            Scaling.Undef_Phys := Cache.Physical_Undef_Value;
         else
            -- FIXME Undef calc is type dependent: if Float Undef is NaN
            -- can be calc'd by Scaling only for (U)Int's
            Scaling.Undef_Phys := Scaling.A + Scaling.B * Cache.Raw_Undef_Value;
            Cache.Raw_Undef_Valid := True;
         end if;
      end if;
   end Load_Undef_Vals_At_Read;


end Cache;
