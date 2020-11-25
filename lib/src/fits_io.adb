
with Ada.Strings.Fixed;
with Ada.Strings; -- Trim needed
use Ada.Strings; -- Left needed for Trim

with Mandatory; -- Result_rec needed
with Optional; -- ReadOtional needed
with Header;    -- Valued_Key_Record_Arr needed

with Ada.Streams.Stream_IO;
with Ada.Text_IO;

with Init;

with File.Misc; -- Padding needed

package body FITS_IO is

   package SIO renames Ada.Streams.Stream_IO;
   package TIO renames Ada.Text_IO;

   ----------------------------------
   -- BEGIN was image/init.ads.adb --
   ----------------------------------

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


   --------------------------------
   -- END was image/init.ads.adb --
   --------------------------------



   ---------------------
   -- File Management --
   ---------------------

   function To_SIO_Mode(Mode : File_Mode) return SIO.File_Mode
   is
   begin
      case(Mode) is
         when In_File => return SIO.In_File;
         when Out_File => return SIO.Out_File;
         when Append_File => return SIO.Append_File;
         end case;
   end To_SIO_Mode;

   function To_FIO_Mode(Mode : SIO.File_Mode) return File_Mode
   is
   begin
      case(Mode) is
         when SIO.In_File => return In_File;
         when SIO.Out_File => return Out_File;
         when SIO.Append_File => return Append_File;
         end case;
   end To_FIO_Mode;

   Null_Access_Rec : constant Access_Rec
      := (BITPIX => 0, A => 0.0, B => 1.0,
      Undef_Used => False, Undef_Raw => 0.0, Undef_Phys => 0.0);

   procedure Create
     (File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := ""; 
      Form : String := "")
   is
   begin
      SIO.Create(File.SIO_File, To_SIO_Mode(Mode), Name, Form);
      -- Init File_Type state
      File.Scaling := Null_Access_Rec;
      File.BITPIX := 0;
      File.Aui := 0.0;
      File.Ah := 0.0;
      File.Bh := 1.0;
      File.Au := 0.0;
      File.Bu := 1.0;
      File.Raw_Undef_Valid := False;
      File.Physical_Undef_Valid := False;
   end Create;

   procedure Open
     (File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "")
   is
   begin
      SIO.Open(File.SIO_File, To_SIO_Mode(Mode), Name, Form);
      -- Init File_Type state
      File.Scaling := Null_Access_Rec;
      File.BITPIX := 0;
      File.Aui := 0.0;
      File.Ah := 0.0;
      File.Bh := 1.0;
      File.Au := 0.0;
      File.Bu := 1.0;
      File.Raw_Undef_Valid := False;
      File.Physical_Undef_Valid := False;
   end Open;

   procedure Close  (File : in out File_Type)
   is
   begin
      SIO.Close(File.SIO_File);
      -- Reset File_Type state
      File.Scaling := Null_Access_Rec;
      File.BITPIX := 0;
      File.Aui := 0.0;
      File.Ah := 0.0;
      File.Bh := 1.0;
      File.Au := 0.0;
      File.Bu := 1.0;
      File.Raw_Undef_Valid := False;
      File.Physical_Undef_Valid := False;
   end Close;

   procedure Reset  (File : in out File_Type; Mode : File_Mode)
   is
   begin
      SIO.Reset(File.SIO_File, To_SIO_Mode(Mode));
   end Reset;

   function Mode(File : File_Type) return File_Mode
   is
   begin
      return To_FIO_Mode(SIO.Mode(File.SIO_File));
   end Mode;

   function End_Of_File (File : File_Type) return Boolean
   is
   begin
      return SIO.End_Of_File(File.SIO_File);
   end End_Of_File;

   function Stream (File : File_Type) return SIO.Stream_Access
   is
   begin
      return SIO.Stream(File.SIO_File);
   end Stream;





   -----------------------------
   -- Input-Output Operations --
   -----------------------------
   procedure Parse_Image_Cards
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



   -- Load File_Type.Access_Rec when Header ready

   procedure Load_BITPIX_And_Scaling_AB(File: in out File_Type)
   is
   begin
      -- A,B interpreted always in Read-direction: Phys = A + B * Raw
      -- inverted values [-A/B , 1/B] used in Write for Undef calc and data scaling
      -- direct values [A,B] used in Read
      File.Scaling.A := File.Ah + File.Au + File.Aui;
      File.Scaling.B := File.Bh * File.Bu;

      File.Scaling.BITPIX := File.BITPIX;
   end Load_BITPIX_And_Scaling_AB;


   procedure Load_Undef_Vals_At_Write(File: in out File_Type)
   is
   begin
      -- Write: Phys -> Raw
      File.Scaling.Undef_Used := File.Physical_Undef_Valid;
      if(File.Scaling.Undef_Used)
      then
         File.Scaling.Undef_Phys := File.Physical_Undef_Value;
         if(File.Raw_Undef_Valid)
         then
            File.Scaling.Undef_Raw := File.Raw_Undef_Value;
         else
            -- FIXME Undef calc is type dependent: if Float Undef is NaN
            -- can be calc'd by Scaling only for (U)Int's
            File.Scaling.Undef_Raw := (File.Raw_Undef_Value - File.Scaling.A) / File.Scaling.B;
         end if;
      end if;
   end Load_Undef_Vals_At_Write;



   procedure Load_Undef_Vals_At_Read(File: in out File_Type)
   is
   begin
      -- Read: Raw -> Phys
      File.Scaling.Undef_Used := File.Raw_Undef_Valid;
      if(File.Scaling.Undef_Used)
      then
         File.Scaling.Undef_Raw := File.Raw_Undef_Value;
         if(File.Physical_Undef_Valid)
         then
            File.Scaling.Undef_Phys := File.Physical_Undef_Value;
         else
            -- FIXME Undef calc is type dependent: if Float Undef is NaN
            -- can be calc'd by Scaling only for (U)Int's
            File.Scaling.Undef_Phys := File.Scaling.A + File.Scaling.B * File.Raw_Undef_Value;
         end if;
      end if;
   end Load_Undef_Vals_At_Read;

   -- RULE Header is read sequentially (because size unknown, must read until END-card)
   -- RULE Data-unit read/write is random access (because size known from the header)

   -- RULE Read_Header funcs (unlike Read_/Write_Cards):
   -- * always reads all header, up to END_Card (incl padding: skip at read)


   -- Cards

   procedure Parse_BITPIX(Cards : String_80_Array; BITPIX : out Integer)
   is
      Found : Boolean := False;
   begin
      for I in Cards'Range
      loop
         Found := (Cards(I)(1..6) = "BITPIX");
         if(Found)
         then
            BITPIX := Integer'Value(Cards(I)(11..30));
         end if;
         exit when Found;
      end loop;
   end Parse_BITPIX;


   function Has_END_Card(Cards : String_80_Array; Pos : out Count) return Boolean
   is
      Found : Boolean := False;
   begin
      Pos := 0;
      for I in Cards'Range
      loop
         Found := (Cards(I) = ENDCard);
         Pos := I;
         exit when Found;
      end loop;
      return Found;
   end Has_END_Card;

   procedure Read
     (File : in out File_Type;
      Item : out String_80_Array;
      Last : out Count)
   is
   begin
      -- FIXME should read by one-card and immediately
      -- check for END-card and skip padding
      -- any further Reads should not move File.Index
      String_80_Array'Read(Stream(File), Item);

      Parse_BITPIX(Item, File.BITPIX);

      Parse_Image_Cards
       (Image_Cards => Item,
       A => File.Ah,
       B => File.Bh,
       Undef_Raw_Valid => File.Raw_Undef_Valid,
       Undef_Raw_Value => File.Raw_Undef_Value);

      if(Has_END_Card(Item,Last))
      then
         -- FIXME skip Padding (File.Index points to DU)
         -- init data unit Access_Rec
         Load_BITPIX_And_Scaling_AB(File);
         Load_Undef_Vals_At_Read(File);
      end if;
   end Read;

   procedure Write
     (File : in out File_Type;
      Item : String_80_Array)
   is
   begin
      String_80_Array'Write(Stream(File), Item);
      Parse_Image_Cards
       (Image_Cards => Item,
       A => File.Ah,
       B => File.Bh,
       Undef_Raw_Valid => File.Raw_Undef_Valid,
       Undef_Raw_Value => File.Raw_Undef_Value);
   end Write;




   function Valued_Card(Key : BS_8.Bounded_String; Value : BS70.Bounded_String) return String_80
   is
   begin
      return Header.Create_Card(BS_8.To_String(Key), BS70.To_String(Value));
   end Valued_Card;


   -- Header


   function BITPIX_To_DU_Type(BITPIX : Integer) return DU_Type
   is
   begin
      case(BITPIX) is
         when  8 => return UInt8;
         when 16 => return Int16;
         when 32 => return Int32;
         when 64 => return Int64;
         when -32 => return F32;
         when -64 => return F64;
         when others => return Uint8; -- FIXME Error invalid BITPIX
      end case;
   end BITPIX_To_DU_Type;




   function  Read_Header
      (FFile   : in out File_Type;
      Keys     : BS_8_Array)  return Image_Rec
   is
      -- FIXME set File.Index to HDUStart (=1)
      Mand : Mandatory.Result_Rec := Header.Read_Mandatory(FFile);
      -- FIXME check HDU_Type -> raise exception if not the expected type
   begin

       File.Set_File_Block_Index(FFile, 1);
      -- FIXME update parser to avoid 2 reads

      declare
         Cards : Optional.Card_Arr := Header.Read_Optional(FFile, Keys);
         Image : Image_Rec(Mand.NAXIS_Last, Cards'Length);
      begin
         Image.Data_Type   := BITPIX_To_DU_Type(Mand.BITPIX);
         Image.NAXISn      := Mand.NAXISn;
         Image.Image_Cards  := Cards;

         -- cache DU-access data

         FFile.BITPIX := Mand.BITPIX;

         Parse_Image_Cards
            (Image_Cards => Cards,
            A => FFile.Ah,
            B => FFile.Bh,
            Undef_Raw_Valid => FFile.Raw_Undef_Valid,
            Undef_Raw_Value => FFile.Raw_Undef_Value);

         -- init data unit Access_Rec
         Load_BITPIX_And_Scaling_AB(FFile);
         Load_Undef_Vals_At_Read(FFile);

         return Image;
      end;

   end Read_Header;



   procedure Write_Image
      (File       : in out File_Type;
      Raw_Type    : DU_Type;
      NAXISn      : NAXIS_Array;
      Image_Cards : String_80_Array)
   is
      BITPIX : Integer;
      Aui : Float;
   begin

      Init.DU_Type_To_BITPIX(Raw_Type, BITPIX, Aui);

      declare

         Cards1 : String_80_Array := (
            Header.Create_Mandatory_Card("BITPIX",Header.To_Value_String(BITPIX)),
            Header.Create_Mandatory_Card("NAXIS", Header.To_Value_String(NAXISn'Length)) );

         Cards : String_80_Array := Cards1 & Header.Create_NAXIS_Card_Arr(NAXISn) & Image_Cards;

         Img : Image_Rec(NAXISn'Last, Image_Cards'Last);

      begin
         Img.Data_Type  := Raw_Type;
         Img.NAXISn     := NAXISn;
         Img.Image_Cards := Image_Cards;

         Write(File, Cards);

         -- cache DU-access data

         File.BITPIX := BITPIX;
         File.Aui := Aui;

         Parse_Image_Cards
            (Image_Cards => Cards,
            A => File.Ah,
            B => File.Bh,
            Undef_Raw_Valid => File.Raw_Undef_Valid,
            Undef_Raw_Value => File.Raw_Undef_Value);

      end;

   end Write_Image;


   -- writes END-card$
   -- writes Padding$
   -- Loads data-unit access params to File.Scaling$
   procedure Write_End(File : in out File_Type)
   is
   begin
      Header.Close(File);
      -- init data unit Access_Rec
      Load_BITPIX_And_Scaling_AB(File);
      Load_Undef_Vals_At_Write(File);
   end Write_End;




   procedure Write_Header
      (File       : in out File_Type;
      Raw_Type    : DU_Type;
      NAXISn      : NAXIS_Array;
      Image_Cards : String_80_Array)
   is
   begin
      Write_Image(File, Raw_Type, NAXISn, Image_Cards);
      Write_End(File);
   end Write_Header;




   -- Data

   procedure Write_Data_Padding(FFile : File_Type)
   is
   begin
            File.Misc.Write_Padding(FFile, Index(FFile), File.Misc.DataPadValue);
   end Write_Data_Padding;


   ----------------------------------------
   -- Operations on Position within File --
   ----------------------------------------

   procedure Set_Index (File : File_Type; To : Positive_Count)
   is
   begin
      SIO.Set_Index(File.SIO_File, SIO.Positive_Count(To));
   end Set_Index;

   function Index (File : File_Type) return Positive_Count
   is
   begin
      return Positive_Count(SIO.Index(File.SIO_File));
   end Index;

   function Size  (File : File_Type) return Count
   is
   begin
      return Positive_Count(SIO.Size(File.SIO_File));
   end Size;


   -- FITS specific

   function Data_Element_Count(NAXISn : NAXIS_Array) return Count
   is
      Data_Cnt : Count := 1;
   begin
      for I in NAXISn'Range
      loop
         Data_Cnt := Data_Cnt * NAXISn(I);
      end loop;
      return Data_Cnt;
   end Data_Element_Count;

   ----------------------------------------------
   -- Converions, Scaling and Undefined Values --
   ----------------------------------------------

   procedure Set_Raw_Type(File : in out File_Type; Raw_Type : DU_Type)
   is
      BITPIX   : Integer;
      Aui      : Float;
   begin
      Init.DU_Type_To_BITPIX(Raw_Type, BITPIX, Aui);
      File.BITPIX := BITPIX;
      File.Aui    := Aui;
   end Set_Raw_Type;


   procedure Set_Linear_Scaling(File : in out File_Type; A,B : Float)
   is
   begin
      File.Au := A;
      File.Bu := B;
   end Set_Linear_Scaling;


   procedure Set_Undefined_Physical(File : in out File_Type; Undef_Phys : Float)
   is
   begin
      File.Physical_Undef_Valid := True;
      File.Physical_Undef_Value := Undef_Phys;
   end Set_Undefined_Physical;


   procedure Put_File_Type(File : File_Type; Prefix : String := "")
   is
   begin
      Put_Access_Rec(File.Scaling,Prefix);
   end Put_File_Type;



end FITS_IO;

