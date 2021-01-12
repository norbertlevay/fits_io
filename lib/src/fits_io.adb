
with Ada.Strings.Fixed;
with Ada.Strings; -- Trim needed
use Ada.Strings; -- Left needed for Trim

with Mandatory; -- Result_rec needed
with Optional; -- ReadOtional needed
with Header;    -- Valued_Key_Record_Arr needed
with Optional; use Optional;-- Bounded_String_8_Arr & Card_Arr needed 
with Ada.Streams.Stream_IO;
with Ada.Text_IO;

with Init;

with File.Misc; -- Padding needed

-- for DataUnit Read/Write:
with Numeric_Type;
with V3_Types; use V3_Types;
with Pool_For_Numeric_Type; use Pool_For_Numeric_Type;
with Array_IO;
with Ada.Exceptions; use Ada.Exceptions;
with FITS_IO.V3_Types_For_DU;

-- FIXME ? T can be of native Ada-types (Long_Long_Integer, Float,...)$
-- and also one of FITS V3-types$
-- Raw can be _only_ FITS V3-type$
-- How to solve Read / Write in case(BITPIX) ... ?
-- Need sizes for native type and do case(T_Bits)...
-- for all: native Types and also FITS-types


package body FITS_IO is

   package SIO renames Ada.Streams.Stream_IO;
   package TIO renames Ada.Text_IO;

   function Read_Content (FFile : in File_Type) return HDU_Info_Type
   is
   begin
      return File.Read_Header(FFile.SIO_File);
   end Read_Content;

   -----------
   -- Utils --
   -----------


   function Valued_Card(Key : BS_8.Bounded_String; Value : BS70.Bounded_String) return String_80
   is
   begin
      return Header.Create_Card(BS_8.To_String(Key), BS70.To_String(Value));
   end Valued_Card;


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



   -------------------------------------------------
   -- Load File_Type.Access_Rec when Header ready --
   -------------------------------------------------


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


   procedure Load_BITPIX_And_Scaling_AB(File: in out File_Type)
   is
   begin
      -- A,B interpreted always in Read-direction: Phys = A + B * Raw
      -- inverted values [-A/B , 1/B] used in Write for Undef calc and data scaling
      -- direct values [A,B] used in Read
      File.Scaling.A := File.Cache.Ah + File.Cache.Au + File.Cache.Aui;
      File.Scaling.B := File.Cache.Bh * File.Cache.Bu;

      File.Scaling.BITPIX := File.Cache.BITPIX;
   end Load_BITPIX_And_Scaling_AB;


   procedure Load_Undef_Vals_At_Write(File: in out File_Type)
   is
   begin
      -- Write: Phys -> Raw
      File.Scaling.Undef_Used := File.Cache.Physical_Undef_Valid;
      if(File.Scaling.Undef_Used)
      then
         File.Scaling.Undef_Phys := File.Cache.Physical_Undef_Value;
         if(File.Cache.Raw_Undef_Valid)
         then
            File.Scaling.Undef_Raw := File.Cache.Raw_Undef_Value;
         else
            -- FIXME Undef calc is type dependent: if Float Undef is NaN
            -- can be calc'd by Scaling only for (U)Int's
            File.Scaling.Undef_Raw :=
               (File.Cache.Physical_Undef_Value - File.Scaling.A) / File.Scaling.B;
               File.Cache.Raw_Undef_Valid := True;
         end if;
      end if;
   end Load_Undef_Vals_At_Write;


   procedure Load_Undef_Vals_At_Read(File: in out File_Type)
   is
   begin
      -- Read: Raw -> Phys
      File.Scaling.Undef_Used := File.Cache.Raw_Undef_Valid;
      if(File.Scaling.Undef_Used)
      then
         File.Scaling.Undef_Raw := File.Cache.Raw_Undef_Value;
         if(File.Cache.Physical_Undef_Valid)
         then
            File.Scaling.Undef_Phys := File.Cache.Physical_Undef_Value;
         else
            -- FIXME Undef calc is type dependent: if Float Undef is NaN
            -- can be calc'd by Scaling only for (U)Int's
            File.Scaling.Undef_Phys := File.Scaling.A + File.Scaling.B * File.Cache.Raw_Undef_Value;
            File.Cache.Raw_Undef_Valid := True;
         end if;
      end if;
   end Load_Undef_Vals_At_Read;




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

   F_Zero : Float := 0.0;
   F_NaN : constant Float := 0.0/F_Zero;
   -- FIXME NaN for Floats in Ada -> how?

   Null_Cache_Rec : constant Cache_Rec
   := (BITPIX => 0,
   Aui => 0.0,
   Ah => 0.0, Bh => 1.0,
   Au => 0.0, Bu => 1.0,
   Physical_Undef_Valid => False,
   Physical_Undef_Value => F_NaN,
   Raw_Undef_Valid => False,
   Raw_Undef_Value => F_NaN);

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
      File.Cache   := Null_Cache_Rec;
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
      File.Cache   := Null_Cache_Rec;
   end Open;

   procedure Close  (File : in out File_Type)
   is
   begin
      SIO.Close(File.SIO_File);
      -- Reset File_Type state
      File.Scaling := Null_Access_Rec;
      File.Cache   := Null_Cache_Rec;
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

   -- RULE Header is read sequentially (because size unknown, must read until END-card)
   -- RULE Data-unit read/write is random access (because size known from the header)

   -- RULE Read_Header funcs (unlike Read_/Write_Cards):
   -- * always reads all header, up to END_Card (incl padding: skip at read)

   -- new API adds cards overwriting current END and adding new END



   -- Read Header

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


   -- API
   function  Read_Header
      (FFile   : in out File_Type;
      Keys     : BS_8_Array)  return Image_Rec
   is
      HDUFirst : SIO.Positive_Count := SIO.Index(FFile.SIO_File);
      Mand : Mandatory.Result_Rec := Header.Read_Mandatory(FFile.SIO_File);
      -- FIXME check HDU_Type -> raise exception if not the expected type
   begin

      -- store begining of DU for DU Read/Write DU_End-guard and padding write
      FFile.DU_First := SIO.Index(FFile.SIO_File);

      --File.Set_File_Block_Index(FFile.SIO_File, 1);
      SIO.Set_Index(FFile.SIO_File, HDUFirst);
      -- FIXME update parser to avoid 2 reads

      declare
         Cards : Optional.Card_Arr := Header.Read_Optional(FFile.SIO_File, Keys);
         Image : Image_Rec(Mand.NAXIS_Last, Cards'Length);
      begin
         Image.Data_Type   := BITPIX_To_DU_Type(Mand.BITPIX);
         Image.NAXISn      := Mand.NAXISn;
         Image.Image_Cards  := Cards;

         -- cache DU-access data

         FFile.Cache.BITPIX := Mand.BITPIX;

         Parse_Image_Cards
            (Image_Cards => Cards,
            A => FFile.Cache.Ah,
            B => FFile.Cache.Bh,
            Undef_Raw_Valid => FFile.Cache.Raw_Undef_Valid,
            Undef_Raw_Value => FFile.Cache.Raw_Undef_Value);

         -- init data unit Access_Rec
         Load_BITPIX_And_Scaling_AB(FFile);
         Load_Undef_Vals_At_Read(FFile);

         FFile.DU_Length := Data_Element_Count(Image.NAXISn);

         return Image;
      end;

   end Read_Header;


   -- API
   function  Read_Cards
      (FFile : in out File_Type;
      Keys   : BS_8_Array)
      return  String_80_Array
   is
      LKeys  : Optional.Bounded_String_8_Arr := Keys;
      LCards : Card_Arr := Header.Read_Optional(FFile.SIO_File, LKeys);
      Cards  : String_80_Array := LCards;
   begin
      return Cards;
   end Read_Cards;





   -- Write Header



   -- writes END-card$
   -- writes Padding$
   -- Loads data-unit access params to File.Scaling$
   procedure Write_End(FFile : in out File_Type)
   is
   begin
      --Header.Close(File);
      FFile.ENDCard_Pos := SIO.Index(FFile.SIO_File);
      String_80'Write(SIO.Stream(FFile.SIO_File), ENDCard);
      File.Misc.Write_Padding(FFile.SIO_File,SIO.Index(FFile.SIO_File),File.Misc.HeaderPadValue);

      FFile.DU_First := SIO.Index(FFile.SIO_File);

      -- init data unit Access_Rec
      Load_BITPIX_And_Scaling_AB(FFile);
      Load_Undef_Vals_At_Write(FFile);
   end Write_End;


   procedure Write_Card_Arr
      (File : in out File_Type;
      Item : String_80_Array)
   is
   begin
      String_80_Array'Write(Stream(File), Item);
      Parse_Image_Cards
         (Image_Cards => Item,
         A => File.Cache.Ah,
         B => File.Cache.Bh,
         Undef_Raw_Valid => File.Cache.Raw_Undef_Valid,
         Undef_Raw_Value => File.Cache.Raw_Undef_Value);
   end Write_Card_Arr;


   procedure Write_Image
      (File       : in out File_Type;
      Raw_Type    : DU_Type;
      NAXISn      : NAXIS_Array;
      Optional_Cards : String_80_Array;
      Is_Primary  : Boolean)-- := True)
   is
      BITPIX : Integer;
      Aui : Float;
   begin

      Init.DU_Type_To_BITPIX(Raw_Type, BITPIX, Aui);

      declare
         Image_Cards_Prim : String_80_Array :=Header.Generate_Primary(BITPIX, NAXISn);
         Image_Cards_Ext  : String_80_Array :=Header.Generate_Conforming_Extension(BITPIX,NAXISn);
      begin

         if(Is_Primary)
         then
            Write_Card_Arr(File, Image_Cards_Prim);
         else
            Write_Card_Arr(File, Image_Cards_Ext);
         end if;
         Write_Card_Arr(File, Optional_Cards);

         -- cache DU-access data

         File.Cache.BITPIX := BITPIX;
         File.Cache.Aui := Aui;

         Parse_Image_Cards
            (Image_Cards => Image_Cards_Prim, -- FIXME not consequent: Prim or Ext ?
            A => File.Cache.Ah,
            B => File.Cache.Bh,
            Undef_Raw_Valid => File.Cache.Raw_Undef_Valid,
            Undef_Raw_Value => File.Cache.Raw_Undef_Value);

         File.DU_Length := Data_Element_Count(NAXISn);

      end;

   end Write_Image;



   procedure Write_First_Image_Card(Out_File : in out File_Type; Is_Primary : Boolean)
   is
      HDU_First_Card : String_80_Array(1 .. 1) :=  
         (1 => Header.Create_Mandatory_Card("SIMPLE", Header.To_Value_String(True)));
      Ext_First_Card : String_80_Array(1 .. 1) :=  
         (1 => Header.Create_Mandatory_Card("XTENSION", "'IMAGE   '"));
      use Ada.Streams.Stream_IO;
   begin
      if(Is_Primary)
      then
         Write_Card_Arr(Out_File, HDU_First_Card);
      else
         Write_Card_Arr(Out_File, Ext_First_Card);
      end if;
   end Write_First_Image_Card;


   -- API
   procedure Write_Header
      (File       : in out File_Type;
      Raw_Type    : DU_Type;
      NAXISn      : NAXIS_Array;
      Image_Cards : String_80_Array)
   is
      use Ada.Streams.Stream_IO;
      Is_Primary : Boolean := (1 = SIO.Index(File.SIO_File));
   begin
      Write_First_Image_Card(File, Is_Primary);
      Write_Image(File, Raw_Type, NAXISn, Image_Cards, Is_Primary);
      Write_End(File);
   end Write_Header;

   -- API
   procedure Write_Cards  -- Add_Cards
      (File       : in out File_Type;
      Cards : String_80_Array)
   is
      Ix : SIO.Positive_Count := 1;
   begin
      -- position File-Index to END card in File header
      SIO.Set_Index(File.SIO_File, File.ENDCard_Pos);
      -- start writing Image_Cards
      String_80_Array'Write(Stream(File), Cards);
      -- add new END card
      Write_End(File); -- writes END-card and padding
   end Write_Cards;




   -- Data


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

   ----------------------------------------------
   -- Converions, Scaling and Undefined Values --
   ----------------------------------------------

   procedure Set_Raw_Type(File : in out File_Type; Raw_Type : DU_Type)
   is
      BITPIX   : Integer;
      Aui      : Float;
   begin
      Init.DU_Type_To_BITPIX(Raw_Type, BITPIX, Aui);
      File.Cache.BITPIX := BITPIX;
      File.Cache.Aui    := Aui;
   end Set_Raw_Type;


   procedure Set_Linear_Scaling(File : in out File_Type; A,B : Float)
   is
   begin
      File.Cache.Au := A;
      File.Cache.Bu := B;
   end Set_Linear_Scaling;


   procedure Set_Undefined_Physical(File : in out File_Type; Undef_Phys : Float)
   is
   begin
      File.Cache.Physical_Undef_Valid := True;
      File.Cache.Physical_Undef_Value := Undef_Phys;
   end Set_Undefined_Physical;


   procedure Put_File_Type(File : File_Type; Prefix : String := "")
   is
   begin
      TIO.Put_Line(Prefix & "Cache Aui = " & Float'Image(File.Cache.Aui));
      Put_Access_Rec(File.Scaling,Prefix);
   end Put_File_Type;



   -- Data Unit


   function Calc_Chunk_Pos
      (F : SIO.File_Type;
      DU_First  : SIO.Positive_Count;
      DU_Length : Positive_Count;
      BITPIX    : Integer; -- bits
      T_Arr_Len : Positive_Count)
      return Count
   is
      N_First : FITS_IO.Positive_Count;
      DU_Current : SIO.Positive_Count := SIO.Index(F);
      SE_Size    : SIO.Positive_Count := Ada.Streams.Stream_Element'Size; -- bits
      use Ada.Streams.Stream_IO;
      T_Size_seu : SIO.Positive_Count := SIO.Positive_Count(abs BITPIX) / SE_Size;
      -- FIXME always divisable ?? -> platform param - seperate out to other ads
   begin
      N_First := FITS_IO.Positive_Count (1 + (DU_Current - DU_First) / T_Size_seu);
      return N_First;
   end Calc_Chunk_Pos;




   procedure Read
      (File    : File_Type;
      Item : out T_Arr;
      Last : out Count)
   is
      type Float_Arr is array (Positive_Count range <>) of Float;
      package Physical is new Numeric_Type(T, T_Arr, Float_Arr);
      use FITS_IO.V3_Types_For_DU;
      package U8_AIO is new Array_IO(U8Raw, Physical);
      package I16_AIO is new Array_IO(I16Raw, Physical);
      package I32_AIO is new Array_IO(I32Raw, Physical);
      package I64_AIO is new Array_IO(I64Raw, Physical);
      package F32_AIO is new Array_IO(F32Raw, Physical);
      package F64_AIO is new Array_IO(F64Raw, Physical);

      Scaling : Access_Rec := File.Scaling;

      -- for padding & detect End_Of_Data_Unit
      Chunk_First : Positive_Count;
      Chunk_Last  : Positive_Count;
      DU_Last     : Positive_Count := File.DU_Length;
      Chunk_Length : Positive_Count;
      Item_Last : Positive_Count;
   begin

      Chunk_First :=Calc_Chunk_Pos(File.SIO_File,File.DU_First,File.DU_Length,abs Scaling.BITPIX,Item'Length);
      Chunk_Last  := Chunk_First + Item'Length - 1;

      -- dont read beyond data unit end
      if(Chunk_First > DU_Last)
      then
         Last := 0;
         return;
      end if;

      -- last chunk hits data unit end -> read only up to end
      -- Chunk_First <= DU_Last
      if(Chunk_Last > DU_Last)
      then
         Chunk_Length := 1 + DU_Last - Chunk_First;
         Item_Last := Item'First + Chunk_Length - 1;
      else
         Item_Last := Item'Last;
      end if;

      Last := Item_Last;


      -- Set Undefined value

      if(Scaling.Undef_Used)
      then

         Physical.Set_Undefined(+Scaling.Undef_Phys);

         case(Scaling.BITPIX) is
            when   8=> U8Raw.Set_Undefined(+Scaling.Undef_Raw);
            when  16=> I16Raw.Set_Undefined(+Scaling.Undef_Raw);
            when  32=> I32Raw.Set_Undefined(+Scaling.Undef_Raw);
            when  64=> I64Raw.Set_Undefined(+Scaling.Undef_Raw);
            when -32=> F32Raw.Set_Undefined(+Scaling.Undef_Raw);
            when -64=> F64Raw.Set_Undefined(+Scaling.Undef_Raw);
            when others =>
               Raise_Exception(Programming_Error'Identity,"BITPIX: "&Integer'Image(Scaling.BITPIX));
         end case;

      end if;


      declare
         Loc_Item : T_Arr(Item'First .. Item_Last);
      begin

         -- Scaling

         case(Scaling.BITPIX) is
            when   8 => U8_AIO.Read(Stream(File), Scaling.A,Scaling.B, Loc_Item);
            when  16 => I16_AIO.Read(Stream(File), Scaling.A,Scaling.B, Loc_Item);
            when  32 => I32_AIO.Read(Stream(File), Scaling.A,Scaling.B, Loc_Item);
            when  64 => I64_AIO.Read(Stream(File), Scaling.A,Scaling.B, Loc_Item);
            when -32 => F32_AIO.Read(Stream(File), Scaling.A,Scaling.B, Loc_Item);
            when -64 => F64_AIO.Read(Stream(File), Scaling.A,Scaling.B, Loc_Item);
            when others =>
               Raise_Exception(Programming_Error'Identity, "BITPIX: "&Integer'Image(Scaling.BITPIX));
         end case;

         Item(Item'First .. Item_Last) := Loc_Item;

      end;

   end Read;



   procedure Write
      (FFile : File_Type;
      Item : T_Arr)
   is
      type Float_Arr is array (Positive_Count range <>) of Float;
      package Physical is new Numeric_Type(T, T_Arr, Float_Arr);
      use FITS_IO.V3_Types_For_DU;
      package U8_AIO is new Array_IO(U8Raw, Physical);
      package I16_AIO is new Array_IO(I16Raw, Physical);
      package I32_AIO is new Array_IO(I32Raw, Physical);
      package I64_AIO is new Array_IO(I64Raw, Physical);
      package F32_AIO is new Array_IO(F32Raw, Physical);
      package F64_AIO is new Array_IO(F64Raw, Physical);

      Scaling : Access_Rec := FFile.Scaling;

      -- for padding & detect End_Of_Data_Unit
      Chunk_First : Positive_Count;
      Chunk_Last  : Positive_Count;
      DU_Last     : Positive_Count := FFile.DU_Length;
      Chunk_Length : Positive_Count;
      Item_Last : Positive_Count;
   begin

      -- dont Write beyond end of Data Unit

      Chunk_First := Calc_Chunk_Pos(FFile.SIO_File,FFile.DU_First,FFile.DU_Length,abs Scaling.BITPIX,Item'Length);
      Chunk_Last  := Chunk_First + Item'Length - 1;

      if(Chunk_First <= DU_Last)
      then

         -- last chunk hits data unit end -> write only up to end
         -- Chunk_First <= DU_Last
         if(Chunk_Last > DU_Last)
         then
            Chunk_Length := 1 + DU_Last - Chunk_First;
            Item_Last := Item'First + Chunk_Length - 1;
         else
            Item_Last := Item'Last;
         end if;


         -- Set Undefined value

         if(Scaling.Undef_Used)
         then

            Physical.Set_Undefined(+Scaling.Undef_Phys);

            case(Scaling.BITPIX) is
               when   8=> U8Raw.Set_Undefined(+Scaling.Undef_Raw);
               when  16=> I16Raw.Set_Undefined(+Scaling.Undef_Raw);
               when  32=> I32Raw.Set_Undefined(+Scaling.Undef_Raw);
               when  64=> I64Raw.Set_Undefined(+Scaling.Undef_Raw);
               when -32=> F32Raw.Set_Undefined(+Scaling.Undef_Raw);
               when -64=> F64Raw.Set_Undefined(+Scaling.Undef_Raw);
               when others =>
                  Raise_Exception(Programming_Error'Identity,
                  "BITPIX: "&Integer'Image(Scaling.BITPIX));
            end case;

         end if;


         declare
            Loc_Item : T_Arr := Item(Item'First .. Item_Last);
         begin

            -- Scaling

            case(Scaling.BITPIX) is
               when   8 =>  U8_AIO.Write(Stream(FFile), Scaling.A,Scaling.B, Loc_Item);
               when  16 => I16_AIO.Write(Stream(FFile), Scaling.A,Scaling.B, Loc_Item);
               when  32 => I32_AIO.Write(Stream(FFile), Scaling.A,Scaling.B, Loc_Item);
               when  64 => I64_AIO.Write(Stream(FFile), Scaling.A,Scaling.B, Loc_Item);
               when -32 => F32_AIO.Write(Stream(FFile), Scaling.A,Scaling.B, Loc_Item);
               when -64 => F64_AIO.Write(Stream(FFile), Scaling.A,Scaling.B, Loc_Item);
               when others =>
                  Raise_Exception(Programming_Error'Identity,
                  "BITPIX: "&Integer'Image(Scaling.BITPIX));
            end case;

         end;

         -- add padding if wrote last data in Data Unit

         if(Chunk_Last >= DU_Last)
         then
            -- Write_Data_Padding(FFile);
            File.Misc.Write_Padding(FFile.SIO_File, SIO.Index(FFile.SIO_File), File.Misc.DataPadValue);
         end if;

      end if;

   end Write;


-- OBSOLETE

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


   procedure OFF_Read_Card_Arr
      (File : in out File_Type;
      Item : out String_80_Array;
      Last : out Count)
   is
   begin
      -- FIXME should read by one-card and immediately
      -- check for END-card and skip padding
      -- any further Reads should not move File.Index
      String_80_Array'Read(Stream(File), Item);

      Parse_BITPIX(Item, File.Cache.BITPIX);

      Parse_Image_Cards
         (Image_Cards => Item,
         A => File.Cache.Ah,
         B => File.Cache.Bh,
         Undef_Raw_Valid => File.Cache.Raw_Undef_Valid,
         Undef_Raw_Value => File.Cache.Raw_Undef_Value);

      if(Has_END_Card(Item,Last))
      then
         -- FIXME skip Padding (File.Index points to DU)
         -- init data unit Access_Rec
         Load_BITPIX_And_Scaling_AB(File);
         Load_Undef_Vals_At_Read(File);
      end if;
   end OFF_Read_Card_Arr;

   procedure OFF_Write_Data_Padding(FFile : File_Type)
   is
   begin
      File.Misc.Write_Padding(FFile.SIO_File, SIO.Index(FFile.SIO_File), File.Misc.DataPadValue);
   end OFF_Write_Data_Padding;



end FITS_IO;

