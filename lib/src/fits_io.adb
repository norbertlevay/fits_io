
with Ada.Strings.Fixed;
with Ada.Strings; -- Trim needed
use Ada.Strings; -- Left needed for Trim

with Mandatory; -- Result_rec needed
with Optional; -- ReadOtional needed
with Header;    -- Valued_Key_Record_Arr needed

with Ada.Streams.Stream_IO;

with Init;

with File.Misc; -- Padding needed

package body FITS_IO is

   package SIO renames Ada.Streams.Stream_IO;


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
      File.Scaling := Null_Access_Rec;
   end Create;

   procedure Open
     (File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "")
   is
   begin
      SIO.Open(File.SIO_File, To_SIO_Mode(Mode), Name, Form);
      File.Scaling := Null_Access_Rec;
   end Open;

   procedure Close  (File : in out File_Type)
   is
   begin
      SIO.Close(File.SIO_File);
      File.Scaling := Null_Access_Rec;
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

   -- Cards

   procedure Read
     (File : File_Type;
      Item : out String_80_Array;
      Last : out Count)
   is
   begin
      String_80_Array'Read(Stream(File), Item);
      -- FIXME updated Access_Rec
   end Read;

   procedure Write
     (File : File_Type;
      Item : String_80_Array)
   is
   begin
      String_80_Array'Write(Stream(File), Item);
      -- FIXME updated Access_Rec
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



   function  Read_Header(FFile : File_Type; Keys : BS_8_Array) return Image_Rec
   is
      Mand : Mandatory.Result_Rec := Header.Read_Mandatory(FFile);
      -- FIXME check HDU_Type -> raise exception if not the expected type
   begin
      -- FIXME updated Access_Rec

       File.Set_File_Block_Index(FFile, 1);
      -- FIXME update parser to avoid 2 reads

      declare
         Cards : Optional.Card_Arr := Header.Read_Optional(FFile, Keys);
         Image : Image_Rec(Mand.NAXIS_Last, Cards'Length);
      begin
         Image.Data_TYpe := BITPIX_To_DU_Type(Mand.BITPIX);
         Image.NAXISn := Mand.NAXISn;
         Image.Array_Keys := Cards;
         return Image;
      end;

   end Read_Header;



   function Write_Image(File : File_Type; DType : DU_Type; NAXISn : NAXIS_Array;
      Array_Keys : String_80_Array)
      return Image_Rec
   is
      BITPIX : Integer;
      Aui : Float;
   begin
      Init.DU_Type_To_BITPIX(DType, BITPIX, Aui);
      -- FIXME update Access_Rec
      declare
         Cards1 : String_80_Array := (
            Header.Create_Mandatory_Card("BITPIX",Header.To_Value_String(BITPIX)),
            Header.Create_Mandatory_Card("NAXIS", Header.To_Value_String(NAXISn'Length)) );
         Cards : String_80_Array := Cards1 & Header.Create_NAXIS_Card_Arr(NAXISn) & Array_Keys;
         Img : Image_Rec(NAXISn'Last, Array_Keys'Last);
      begin
         Img.Data_Type := DType;
         Img.NAXISn := NAXISn;
         Img.Array_Keys := Array_Keys;
         Write(File, Cards);
         return Img;
      end;
   end Write_Image;

   procedure Write_End(File : File_Type)
   is
   begin
      Header.Close(File);
      -- FIXME update Access_Rec
   end Write_End;


   -- Data


   procedure Create
      (Data_Unit : in out Data_Unit_Type;
      File       : File_Type;
      Image      : Image_Rec;
      Phys_Used  : Boolean := False;
      Phys_Value : Float := 0.0;
      A : Float := 0.0;
      B : Float := 1.0)
   is
      FMode : File_Mode := Mode(File);
      BITPIX : Integer;
      Aui : Float;
      Raw_Valid : Boolean := False;
      Raw_Value : Float;
   begin
      case(FMode) is
         when Append_File =>

            -- parse BLANK -> Undef_Raw...
            for I in Image.Array_Keys'Range
            loop
               if( Image.Array_Keys(I)(1..5) = "BLANK")
               then
                  Raw_Value := Float'Value(Image.Array_Keys(I)(11..30));
                  Raw_Valid := True;
               end if;
            end loop;

            Init.DU_Type_To_BITPIX(Image.Data_Type, BITPIX, Aui);

            Init.Init_Writes
               (BITPIX => BITPIX,
               Undef_Phys_Used => Phys_Used,
               Undef_Phys      => Phys_Value,
               A => A + Aui,
               B => B,
               Undef_Raw_Valid => Raw_Valid,
               Undef_Raw       => Raw_Value,
               DU_Access => Data_Unit.Scaling);

            Init.Put_Access_Rec(Data_Unit.Scaling);

         when others => null; -- error : Create invalid in this Mode FIXME
      end case;
   end Create;




   procedure Open
      (Data_Unit : in out Data_Unit_Type;
      File       : File_Type;
      Image      : Image_Rec;
      Phys_Used  : Boolean := False;
      Phys_Value : Float := 0.0;
      A : Float := 0.0;
      B : Float := 1.0)
   is
      FMode   : File_Mode := Mode(File);
      BITPIX : Integer;
      Aui : Float;
   begin

      Init.DU_Type_To_BITPIX(Image.Data_Type, BITPIX, Aui);

      case(FMode) is
         when In_File =>

            Init.Init_Reads
               (BITPIX => BITPIX,
               Array_Keys => Image.Array_Keys,
               A => A + Aui,
               B => B,
               Undef_Phys_Valid => Phys_Used,
               Undef_Phys       => Phys_Value,
               DU_Access => Data_Unit.Scaling);

            Init.Put_Access_Rec(Data_Unit.Scaling);

         when Out_File =>

            -- from Image.Array_Keys parse BLANK -> Undef_Raw...

            Init.Init_Writes
               (BITPIX => BITPIX,
               Undef_Phys_Used => Phys_Used,
               Undef_Phys      => Phys_Value,
               A => A + Aui,
               B => B,
               Undef_Raw_Valid => False,
               Undef_Raw       => 0.0,
               DU_Access => Data_Unit.Scaling);

         when others => null; -- error : Open invalid in this Mode FIXME
      end case;
   end Open;




   procedure Close
      (Data_Unit : in out Data_Unit_Type;
      FFile : File_Type)
   is
      FMode : File_Mode := Mode(FFile);
   begin
      -- FIXME this approach works only if sequential access (write) allowed
      -- for random access the File.Index could point anywhere within DU
      -- for squential access it is assumed caller calls Close only when all data written
      -- and in that case File.Index points correctly
      case(FMode) is
         when In_File =>
            null;-- reset Data_Unit_Type
         when Out_File | Append_File =>
            File.Misc.Write_Padding(FFile, Index(FFile), File.Misc.DataPadValue);
      end case;
   end Close;


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
      BITPIX : Integer;
      Aui : Float;
   begin
      Init.DU_Type_To_BITPIX(Raw_Type, BITPIX, Aui);
      File.Scaling.BITPIX := BITPIX;
      File.Scaling.A := File.Scaling.A + Aui;
   end Set_Raw_Type;

   procedure Set_Linear_Scaling(File : in out File_Type; A,B : Float)
   is
   begin
      File.Scaling.A := File.Scaling.A + A;
      File.Scaling.B := File.Scaling.B * B;
      -- FIXME overwrite or combine as above ??
   end Set_Linear_Scaling;

   procedure Set_Undefined_Values(File : in out File_Type; Undef_Raw, Undef_Phys : Float)
   is
   begin
      File.Scaling.Undef_Used := True;
      File.Scaling.Undef_Raw := Undef_Raw;
      File.Scaling.Undef_Raw := Undef_Phys;
   end Set_Undefined_Values;


   function Get(File : File_Type) return Access_Rec
   is
   begin
      return File.Scaling;
   end Get;

end FITS_IO;

