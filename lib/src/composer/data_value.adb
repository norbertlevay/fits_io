

--with Header;
with Optional;

package body Data_Value is


   procedure Parse_Image_Cards -- alg
      (Image_Cards : in String_80_Array;
      A : out Float;
      B : out Float;
      Undef_Raw_Valid : in out Boolean;
      Undef_Raw_Value : out Float)
   is  
   begin
      -- init
      A := 0.0; B:= 1.0;
      -- overwrite inited if exists
      for I in Image_Cards'Range
      loop
         if(Image_Cards(I)(1 .. 5) = "BZERO")
         then
            A := Float'Value(Image_Cards(I)(11 .. 30));
         elsif(Image_Cards(I)(1 .. 6) = "BSCALE")
         then
            B := Float'Value(Image_Cards(I)(11 .. 30));
         elsif(Image_Cards(I)(1 .. 5) = "BLANK")
         then
            Undef_Raw_Valid := True;
            Undef_Raw_Value := Float'Value(Image_Cards(I)(11 .. 30));
         end if;
      end loop;
   end Parse_Image_Cards;



   function To_Array_Keys(DU_Access : Access_Rec) return Optional.Valued_Key_Record_Arr -- alg
   is
      Ncards : Natural := 0;
      KeysBuffer : Optional.Valued_Key_Record_Arr(1..3);
      use Optional.BS_8;
      use Optional.BS70;
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
         ArrKeys : Optional.Valued_Key_Record_Arr := KeysBuffer(1..Ncards);
      begin
         return ArrKeys;
      end;
   end To_Array_Keys;




end Data_Value;
