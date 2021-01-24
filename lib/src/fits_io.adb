
-- FIXME ? T can be of native Ada-types (Long_Long_Integer, Float,...)$
-- and also one of FITS V3-types$
-- Raw can be _only_ FITS V3-type$
-- How to solve Read / Write in case(BITPIX) ... ?
-- Need sizes for native type and do case(T_Bits)...
-- for all: native Types and also FITS-types


with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Deallocation;-- for HDU Stream

-- for File_Type / Stream
with Header; -- Read Mandatory / Optional needed
with Cache;

-- for Header Parse/Compose
with DU_Types;
with Mandatory; -- Result_Rec needed
with Elements;

-- for Data Unit Read/Write
with Numeric_Type;
with V3_Types; use V3_Types;
with Pool_For_Numeric_Type; use Pool_For_Numeric_Type;
with Array_IO;
with FITS_IO.V3_Types_For_DU;

with File.Misc; -- DataPadding needed

with DU_Pos;


package body FITS_IO is

   package SIO renames Ada.Streams.Stream_IO;
   package TIO renames Ada.Text_IO;

   -- API
   function Read_Content (FFile : in File_Type) return HDU_Info_Type
   is
   begin
      return File.Read_Header(FFile.SIO_File);
   end Read_Content;


   -----------
   -- Utils --
   -----------


   function Data_Element_Count(NAXISn : NAXIS_Array) return Count -- alg
   is
      Data_Cnt : Count := 1;
   begin
      for I in NAXISn'Range
      loop
         Data_Cnt := Data_Cnt * NAXISn(I);
      end loop;
      return Data_Cnt;
   end Data_Element_Count;


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


   ---------------------
   -- File Management --
   ---------------------

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
      Has_Data_Padding : Boolean := False;
   begin
      SIO.Close(File.SIO_File);
      -- Reset File_Type state
      File.Scaling := Null_Access_Rec;
      File.Cache   := Null_Cache_Rec;
      SIO_DU_Pos.Is_Data_Padding_Written(Has_Data_Padding);
      if(not Has_Data_Padding)
      then
         null;-- FIXME error: Fits_File invalid, Data Unit incomplete
      end if;
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

      FFile.Pos.HDU_First := HDUFirst; -- FIXME  should be param to Read_Header not in DU_Pos ??
      FFile.Pos.DU_First  := SIO.Index(FFile.SIO_File);

      FFile.DU_First := SIO.Index(FFile.SIO_File);


      SIO.Set_Index(FFile.SIO_File, HDUFirst);
      -- FIXME update parser to avoid 2 reads

      declare
         Cards : String_80_Array := Header.Read_Optional(FFile.SIO_File, Keys);
         Image : Image_Rec(Mand.NAXIS_Last, Cards'Length);
      begin
         Image.Data_Type   := DU_Types.BITPIX_To_DU_Type(Mand.BITPIX);
         Image.NAXISn      := Mand.NAXISn;
         Image.Image_Cards := Cards;

         -- cache DU-access data

         FFile.Cache.BITPIX := Mand.BITPIX;

         Cache.Parse_Image_Cards
            (FFile.Cache,
            Cache.String_80_Array(Cards)); -- FIXME conversion!

         -- init data unit Access_Rec
         Load_BITPIX_And_Scaling_AB(FFile.Scaling, FFile.Cache);
         Load_Undef_Vals_At_Read   (FFile.Scaling, FFile.Cache);

         FFile.DU_Length := Data_Element_Count(Mand.NAXISn);

         return Image;
      end;

   end Read_Header;


   -- API
   function  Read_Cards
      (FFile : in out File_Type;
      Keys   : BS_8_Array)
      return  String_80_Array
   is
      LKeys  : BS_8_Array := Keys;
      -- FIXME Set_Index at FFile.Pos.HDU_First -> Write_Cards does set at ENDCCard
      LCards : String_80_Array := Header.Read_Optional(FFile.SIO_File, LKeys);
      Cards  : String_80_Array := LCards;
   begin
      return Cards;
   end Read_Cards;


   -- Write Header


   procedure Write_End(FFile : in out File_Type)
   is
   begin

      FFile.ENDCard_Pos := SIO.Index(FFile.SIO_File);
      FFile.Pos.ENDCard_Pos := SIO.Index(FFile.SIO_File);
      Header.Write_ENDCard_With_Padding(FFile.SIO_File);
      FFile.DU_First := SIO.Index(FFile.SIO_File);
      FFile.Pos.DU_First := SIO.Index(FFile.SIO_File);

      -- init data unit Access_Rec

      Load_BITPIX_And_Scaling_AB(FFile.Scaling, FFile.Cache);
      Load_Undef_Vals_At_Write  (FFile.Scaling, FFile.Cache);

   end Write_End;


   procedure Write_Card_Arr
      (File : in out File_Type;
      Item : String_80_Array)
   is
   begin
      String_80_Array'Write(Stream(File), Item);
      Cache.Parse_Image_Cards
         (File.Cache,
         Cache.String_80_Array(Item)); -- FIXME conversion!
   end Write_Card_Arr;


   procedure Write_Image
      (File       : in out File_Type;
      Raw_Type    : DU_Type;
      NAXISn      : NAXIS_Array;
      Optional_Cards : String_80_Array;
      Is_Primary  : Boolean)
   is
      BITPIX : Integer;
      Aui : Float;
   begin

      DU_Types.DU_Type_To_BITPIX(Raw_Type, BITPIX, Aui);

      declare
         Im_Prim : Elements.Primary              := (NAXISn'Last, BITPIX, NAXISn);
         Im_Ext  : Elements.Conforming_Extension := (NAXISn'Last, BITPIX, NAXISn, 0, 1);
         Optional_Cards_Prim : String_80_Array := Elements.Generate_Cards(Im_Prim);
         Optional_Cards_Ext  : String_80_Array := Elements.Generate_Cards(Im_Ext);
      begin

         if(Is_Primary)
         then
            Write_Card_Arr(File, Optional_Cards_Prim);
         else
            Write_Card_Arr(File, Optional_Cards_Ext);
         end if;

         Write_Card_Arr(File, Optional_Cards);

         -- cache DU-access data

         File.Cache.BITPIX := BITPIX;
         File.Cache.Aui := Aui;

         File.DU_Length := Data_Element_Count(NAXISn);

      end;

   end Write_Image;


   -- API
   procedure Write_Header_Prim -- Compose_Header
      (File       : in out File_Type;
      Raw_Type    : DU_Type;
      NAXISn      : NAXIS_Array;
      Optional_Cards : String_80_Array)
   is
      use Ada.Streams.Stream_IO;
      HDUFirst : SIO.Positive_Count := SIO.Positive_Count(SIO.Index(File.SIO_File));
      Is_Primary : Boolean := (HDUFirst = 1);
      Prim_First_Card : String_80_Array := Elements.Create_Card_SIMPLE(True);
   begin
      File.Pos.HDU_First := HDUFirst;
      Write_Card_Arr(File, Prim_First_Card);
      Write_Image(File, Raw_Type, NAXISn, Optional_Cards, Is_Primary);
      Write_End(File);
   end Write_Header_Prim;

   -- FIXME should HDUFirst be managed outside of DU_Pos ? Write_Header_*()

   -- API
   procedure Write_Header_Ext -- Compose_Header
      (File       : in out File_Type;
      Raw_Type    : DU_Type;
      NAXISn      : NAXIS_Array;
      Optional_Cards : String_80_Array)
   is
      use Ada.Streams.Stream_IO;
      HDUFirst : SIO.Positive_Count := SIO.Index(File.SIO_File);
      Is_Primary : Boolean := (HDUFirst = 1);
      Ext_First_Card  : String_80_Array := Elements.Create_Card_XTENSION("'IMAGE   '");
   begin
      File.Pos.HDU_First := HDUFirst;
      Write_Card_Arr(File, Ext_First_Card);
      Write_Image(File, Raw_Type, NAXISn, Optional_Cards, Is_Primary);
      Write_End(File);
   end Write_Header_Ext;



   -- API
   procedure Write_Cards  -- Add_Cards
      (File       : in out File_Type;
      Cards : String_80_Array)
   is
      Ix : SIO.Positive_Count := 1;
   begin
      -- position File-Index at END-card
      SIO.Set_Index(File.SIO_File, File.ENDCard_Pos); -- FIXME use File.Pos.ENDCard_Pos
      Write_Card_Arr(File, Cards);  -- start writing Cards (overwrites existing ENDCard)
      Write_End(File);              -- writes new END-card and padding
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
      DU_Types.DU_Type_To_BITPIX(Raw_Type, BITPIX, Aui);
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


   ----------------------
   -- Data Unit access --
   ----------------------


   function Calc_Chunk_Pos -- alg
      (F : SIO.File_Type;
      DU_First  : SIO.Positive_Count;
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




   procedure HDU_Read -- alg
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

      Chunk_First :=Calc_Chunk_Pos(File.SIO_File,File.DU_First,abs Scaling.BITPIX,Item'Length);
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

   end HDU_Read;



   procedure HDU_Write -- alg
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

      Chunk_First := Calc_Chunk_Pos(FFile.SIO_File,FFile.DU_First,abs Scaling.BITPIX,Item'Length);
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

   end HDU_Write;


   ----------------
   -- HDU Stream --
   ----------------

   function AFCB_Allocate (Control_Block : HDU_Stream_AFCB) return FCB.AFCB_Ptr is
      pragma Warnings (Off, Control_Block);
   begin
      return new HDU_Stream_AFCB;
   end AFCB_Allocate;

   procedure AFCB_Close (File : not null access HDU_Stream_AFCB) is
      pragma Warnings (Off, File);
   begin
      null;
   end AFCB_Close;

   procedure AFCB_Free (File : not null access HDU_Stream_AFCB) is
      type FCB_Ptr is access all HDU_Stream_AFCB;
      FT : FCB_Ptr := FCB_Ptr (File);
      procedure Free is new Ada.Unchecked_Deallocation (HDU_Stream_AFCB, FCB_Ptr);
   begin
      Free (FT);
   end AFCB_Free;

   --  This version of Read is the primitive operation on the underlying
   --  Stream type, used when a Stream_IO file is treated as a Stream

   procedure Read
      (File : in out HDU_Stream_AFCB;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
   begin
      null;-- FIXME implement
      --HDU_Read (File'Unchecked_Access, Item, Last);
   end Read;

   --  This version of Write is the primitive operation on the underlying
   --  Stream type, used when a Stream_IO file is treated as a Stream

   procedure Write
      (File : in out HDU_Stream_AFCB;
      Item : Ada.Streams.Stream_Element_Array)
   is
   begin
      null; -- FIXME implement
      --HDU_Write (File'Unchecked_Access, Item);
   end Write;


end FITS_IO;

