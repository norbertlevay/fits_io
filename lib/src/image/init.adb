
with Optional;

with Ada.Text_IO;

with Interfaces; use Interfaces; -- or use V3_Types

with Ada.Strings.Bounded;

package body Init is

   package TIO renames Ada.Text_IO;





   procedure DU_Type_To_BITPIX
      (DUType : DU_Type;
      BITPIX  : out Integer;
      Aui     : out Float)
   is
   begin
      -- implements Tab11 FITSv3
      case(DUType) is
         when   Int8 => Aui :=                -128.0; BITPIX :=   8; -- -2**7
         when  UInt8 => Aui :=                   0.0; BITPIX :=   8;
         when  Int16 => Aui :=                   0.0; BITPIX :=  16;
         when UInt16 => Aui :=               32768.0; BITPIX :=  16; --  2**15
         when  Int32 => Aui :=                   0.0; BITPIX :=  32;
         when UInt32 => Aui :=          2147483648.0; BITPIX :=  32; --  2**31
         when  Int64 => Aui :=                   0.0; BITPIX :=  64;
         when UInt64 => Aui := 9223372036854775808.0; BITPIX :=  64; --  2**63
         when    F32 => Aui :=                   0.0; BITPIX := -32;
         when    F64 => Aui :=                   0.0; BITPIX := -64;
      end case;
   end DU_Type_To_BITPIX;


   -- DU_Type & DATAMIN DATAMAX -> [A, B]

   procedure DU_Type_Min_Max(DUIntType : in DU_Int_Type; Min, Max : out Float)
   is
   begin
      case(DUIntType) is
         when  Int8  => Min := Float( Integer_8'First);  Max := Float( Integer_8'Last);
         when UInt8  => Min := Float(Unsigned_8'First);  Max := Float(Unsigned_8'Last);
         when  Int16 => Min := Float( Integer_16'First); Max := Float( Integer_16'Last);
         when UInt16 => Min := Float(Unsigned_16'First); Max := Float(Unsigned_16'Last);
         when  Int32 => Min := Float( Integer_32'First); Max := Float( Integer_32'Last);
         when UInt32 => Min := Float(Unsigned_32'First); Max := Float(Unsigned_32'Last);
         when  Int64 => Min := Float( Integer_64'First); Max := Float( Integer_64'Last);
         when UInt64 => Min := Float(Unsigned_64'First); Max := Float(Unsigned_64'Last);
         when F32 | F64 => null; -- FIXME DU_Int_Type but compiler complains missing F32 F64 ?!
      end case;
   end DU_Type_Min_Max;


   procedure Linear_Scale
      (DUIntType : in DU_Int_Type;
      DATAMIN, DATAMAX : in Float;
      A : out Float; B : out Float)
   is
      Min, Max : Float;
   begin

     if ((DATAMAX - DATAMIN) = 0.0)
     then
        null;-- Programming Error
     end if;

     DU_Type_Min_Max(DUIntType, Min, Max);

     B := (Max - Min) / (DATAMAX - DATAMIN);
     A := Max - B * DATAMAX;

   end Linear_Scale;



   -- FIXME replace with one implementation
   -- Undef_Init(A,B, Undef_1_Used, Undef_1, Undef_2_Valid, Undef_2, Undef_Rec out)
   -- and swap Undef_1 <-> Undef_2 for Read Write
   -- and for write: A => -A/B  B => 1/B

   procedure Undef_Init_For_Reads
      (A, B : in Float;
      Undef_Raw_Used   : in Boolean;
      Undef_Raw        : in Float;
      Undef_Phys_Valid : in Boolean := False;
      Undef_Phys       : in Float   := 0.0;
      DU_Access : out Access_Rec)
  is
  begin

      DU_Access.Undef_Used := Undef_Raw_Used;

      if(DU_Access.Undef_Used)
      then

         DU_Access.Undef_Raw := Undef_Raw;

         if(Undef_Phys_Valid)
         then
            DU_Access.Undef_Phys := Undef_Phys;
         else
            DU_Access.Undef_Phys := A + B * DU_Access.Undef_Raw;
         end if;

      end if;

      DU_Access.A := A;
      DU_Access.B := B;

   end Undef_Init_For_Reads;


   procedure Undef_Init_For_Writes
      (A, B : in Float; -- B /= 0
      Undef_Phys_Used : in Boolean;
      Undef_Phys      : in Float;
      Undef_Raw_Valid : in Boolean := False;
      Undef_Raw       : in Float   := 0.0;
      DU_Access : out Access_Rec)
   is
   begin

      DU_Access.Undef_Used := Undef_Phys_Used;

      if(DU_Access.Undef_Used)
      then

         DU_Access.Undef_Phys := Undef_Phys;

         if(Undef_Raw_Valid)
         then
            DU_Access.Undef_Raw := Undef_Raw;
         else
            DU_Access.Undef_Raw := (DU_Access.Undef_Phys - A) / B;
         end if;

      end if;

      DU_Access.A := A;
      DU_Access.B := B;

   end Undef_Init_For_Writes;



   procedure Init_Reads
      (BITPIX : in Integer;
      Image_Cards : in String_80_Array;
      A           : in Float := 0.0;
      B           : in Float := 1.0;
      Undef_Phys_Valid : in Boolean := False;
      Undef_Phys       : in Float   := 0.0;
      DU_Access   : out Access_Rec)
   is
--      BITPIX : Integer;
--      Aui : Float; -- Tab11 UInt-Int conversion shift
      Ah : Float := 0.0; -- A,B from Header BZERO BSCALE
      Bh : Float := 1.0; -- A,B from Header BZERO BSCALE
      use Optional.BS_8;
      Undef_Raw_Used : Boolean := False;
      Undef_Raw : Float;
      Aall, Ball : Float;
   begin

      -- calc [A,B]

      for I in Image_Cards'Range
      loop
         if(Image_Cards(I)(1 .. 5) = "BZERO")
         then
            Ah := Float'Value(Image_Cards(I)(11 .. 30));
         elsif(Image_Cards(I)(1 .. 6) = "BSCALE")
         then
            Bh := Float'Value(Image_Cards(I)(11 .. 30));
         elsif(Image_Cards(I)(1 .. 5) = "BLANK")
         then
            Undef_Raw_Used := True;
            Undef_Raw := Float'Value(Image_Cards(I)(11 .. 30));
        end if;
      end loop;

--      DU_Type_To_BITPIX(Raw_Type, BITPIX, Aui);

      Aall := A + Ah;-- + Aui;
      Ball := B * Bh;

      -- calc Undef

      Undef_Init_For_Reads(Aall, Ball,
            Undef_Raw_Used,
            Undef_Raw,
            Undef_Phys_Valid,
            Undef_Phys,
            DU_Access);

      DU_Access.BITPIX := BITPIX;

   end Init_Reads;



   procedure Init_Writes
      (BITPIX: in Integer;
      Image_Cards : in String_80_Array;
      Undef_Phys_Used : in Boolean;
      Undef_Phys      : in Float;
      A               : in Float := 0.0;
      B               : in Float := 1.0;
      DU_Access       : out Access_Rec)
   is
--      BITPIX : Integer;
--      Aui    : Float; -- Tab11 UInt-Int conversion shift
      Undef_Raw_Valid : Boolean := False;
      Undef_Raw       : Float   := 0.0;
      Aall, Ball : Float;
      Ah, Bh : Float;
   begin

      -- calc [A,B]

      for I in Image_Cards'Range
      loop
         if(Image_Cards(I)(1 .. 5) = "BZERO")
         then
            Ah := Float'Value(Image_Cards(I)(11 .. 30));
         elsif(Image_Cards(I)(1 .. 6) = "BSCALE")
         then
            Bh := Float'Value(Image_Cards(I)(11 .. 30));
         elsif(Image_Cards(I)(1 .. 5) = "BLANK")
         then
            Undef_Raw_Valid := True;
            Undef_Raw := Float'Value(Image_Cards(I)(11 .. 30));
        end if;
      end loop;


--      DU_Type_To_BITPIX(Raw_Type, BITPIX, Aui);

      Aall := Ah + A;-- + Aui;
      Ball := Bh * B;

      -- calc Undef

      Undef_Init_For_Writes(Aall, Ball,
         Undef_Phys_Used,
         Undef_Phys,
         Undef_Raw_Valid,
         Undef_Raw,
         DU_Access);

      DU_Access.BITPIX := BITPIX;

   end Init_Writes;




   -- convert Access_Rec - Reserved.Array_Keys cards

   function To_Array_Keys(DU_Access : Access_Rec) return Header.Valued_Key_Record_Arr
   is
      Ncards : Natural := 0;
      KeysBuffer : Header.Valued_Key_Record_Arr(1..3);
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
         ArrKeys : Header.Valued_Key_Record_Arr := KeysBuffer(1..Ncards);
      begin
         return ArrKeys;
      end;
   end To_Array_Keys;



   -- utils
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

  procedure Put_Array_Keys(Keys : Header.Valued_Key_Record_Arr; Prefix : String := "")
  is  
     use Optional.BS_8;
     use Optional.BS70;
  begin
     for I in Keys'Range
      loop
         TIO.Put_Line(Prefix & To_String(Keys(I).Key) & " " & To_String(Keys(I).Value));
      end loop;
  end Put_Array_Keys;




end Init;
