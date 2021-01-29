
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

with FITS; use FITS;

package body FITS_IO is

   --package SIO renames Ada.Streams.Stream_IO;
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
      File.SIO_HDU_First := SIO.Index(File.SIO_File);
      HDU.Reset(File.PHDU, File.SIO_HDU_First);
   end Create;

   procedure Open
      (File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "")
   is
   begin
      SIO.Open(File.SIO_File, To_SIO_Mode(Mode), Name, Form);
      File.SIO_HDU_First := SIO.Index(File.SIO_File);
      HDU.Reset(File.PHDU, File.SIO_HDU_First);
   end Open;

   procedure Close  (File : in out File_Type)
   is
   begin
      HDU.Write_Data_Unit_Padding(File.SIO_File);
      SIO.Close(File.SIO_File);
      HDU.Reset(File.PHDU, 1);
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


   -- API but later hide behind Open
   function  Read_Header
      (FFile   : in out File_Type;
      Keys     : BS_8_Array)  return Image_Rec
   is
   begin
      return HDU.Read_Header(FFile.SIO_File, FFile.PHDU, Keys);
   end Read_Header;


   -- API on level of Open which calls Set_Index(HDUFirst)
   function  Read_Cards
      (FFile : in out File_Type;
      Keys   : BS_8_Array)
      return  String_80_Array
   is
   begin
        return HDU.Read_Cards(FFile.SIO_File, FFile.PHDU, Keys);
   end Read_Cards;


   -- Write Header


   -- API later hide behind Create / Open(Out_Mode)
   procedure Write_Header_Prim -- Compose_Header
      (File       : in out File_Type;
      Raw_Type    : DU_Type;
      NAXISn      : NAXIS_Array;
      Optional_Cards : String_80_Array)
   is
  begin
      HDU.Write_Header_Prim(File.SIO_File, File.PHDU, Raw_Type, NAXISn, Optional_Cards);
  end Write_Header_Prim;


   -- API later hide behind Open(Append_Mode)
   procedure Write_Header_Ext -- Compose_Header
      (File       : in out File_Type;
      Raw_Type    : DU_Type;
      NAXISn      : NAXIS_Array;
      Optional_Cards : String_80_Array)
   is
   begin
      HDU.Write_Header_Ext(File.SIO_File, File.PHDU, Raw_Type, NAXISn, Optional_Cards);
   end Write_Header_Ext;



   -- API on level od Open/Create; later rename to Append_Cards
   procedure Write_Cards
      (File       : in out File_Type;
      Cards : String_80_Array)
   is
   begin
      HDU.Write_Cards(File.SIO_File, File.PHDU, Cards);
   end Write_Cards;




   -- Data


   ----------------------------------------
   -- Operations on Position within File --
   ----------------------------------------

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
      File.PHDU.Cache.BITPIX := BITPIX;
      File.PHDU.Cache.Aui    := Aui;
   end Set_Raw_Type;


   procedure Set_Linear_Scaling(File : in out File_Type; A,B : Float)
   is
   begin
      File.PHDU.Cache.Au := A;
      File.PHDU.Cache.Bu := B;
   end Set_Linear_Scaling;


   procedure Set_Undefined_Physical(File : in out File_Type; Undef_Phys : Float)
   is
   begin
      File.PHDU.Cache.Physical_Undef_Valid := True;
      File.PHDU.Cache.Physical_Undef_Value := Undef_Phys;
   end Set_Undefined_Physical;


   procedure Put_File_Type(File : File_Type; Prefix : String := "")
   is
   begin
      TIO.Put_Line(Prefix & "Cache Aui = " & Float'Image(File.PHDU.Cache.Aui));
      Put_Access_Rec(File.PHDU.Scaling,Prefix);
   end Put_File_Type;


   ----------------------
   -- Data Unit access --
   ----------------------


   -- Random access Index : 1 .. DU_Last

   -- FIXME stop compile on system where DE_Site SE_Size is not divisible
   -- or how to avoid DE/SE & SE/DE divisions ?

   function  Index(File : File_Type) return Positive_Count
   is
      SIO_Index  : SIO.Positive_Count := SIO.Index(File.SIO_File);
   begin
      return DU_Pos.DU_Index(SIO_Index, File.PHDU.Pos.SIO_DU_First, File.PHDU.Scaling.BITPIX);
   end Index;


   procedure Set_Index(File : File_Type; Ix : Positive_Count)
   is
      SIO_Index : SIO.Positive_Count;
      use SIO;
      HDU_Inited : Boolean:=(File.PHDU.Pos.SIO_DU_First /= 0) AND (File.PHDU.Scaling.BITPIX /= 0);
   begin
      if(HDU_Inited)
      then
         SIO_Index := DU_Pos.SE_Index(Ix,
                                      File.PHDU.Pos.SIO_DU_First, File.PHDU.Scaling.BITPIX);
         SIO.Set_Index(File.SIO_File, SIO_Index);
      else
         null; -- FIXME programming error: Set_Index called but HDU is empty
      end if;
   end Set_Index;



   -- util FIXME get from some lib or dont use

   function Min(A,B : Count) return Count
   is  
   begin
      if (A > B) then return B; else return A; end if;
   end Min;


   -- Data access

   procedure HDU_Read -- alg
      (FFile    : in out File_Type;
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

      Scaling : Access_Rec := FFile.PHDU.Scaling;

      -- for padding & detect End_Of_Data_Unit

      DU_Curr_Ix : Positive_Count := Index(FFile);
      DU_Last : constant Positive_Count := Positive_Count(DU_Pos.Get_DU_Last(FFile.PHDU.Pos));
      DU_Item_Last : Positive_Count;
   begin

      if(DU_Curr_Ix > DU_Last ) then null; end if;-- FIXME raise error End-Of-DataUnit$

      DU_Item_Last := DU_Curr_Ix + Item'Length - 1;

      Last := Min(Item'Last, 1 + DU_Item_Last - DU_Curr_Ix);

      --      Is_Last_Write := (DU_Item_Last >= DU_Last);

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
         Loc_Item : T_Arr(Item'First .. Last);
      begin

         -- Scaling

         case(Scaling.BITPIX) is
            when   8 => U8_AIO.Read(Stream(FFile), Scaling.A,Scaling.B, Loc_Item);
            when  16 => I16_AIO.Read(Stream(FFile), Scaling.A,Scaling.B, Loc_Item);
            when  32 => I32_AIO.Read(Stream(FFile), Scaling.A,Scaling.B, Loc_Item);
            when  64 => I64_AIO.Read(Stream(FFile), Scaling.A,Scaling.B, Loc_Item);
            when -32 => F32_AIO.Read(Stream(FFile), Scaling.A,Scaling.B, Loc_Item);
            when -64 => F64_AIO.Read(Stream(FFile), Scaling.A,Scaling.B, Loc_Item);
            when others =>
               Raise_Exception(Programming_Error'Identity, "BITPIX: "&Integer'Image(Scaling.BITPIX));
         end case;

         Item(Item'First .. Last) := Loc_Item;

      end;

   end HDU_Read;



   procedure HDU_Write -- alg
      (FFile : in out File_Type;
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

      Scaling : Access_Rec := FFile.PHDU.Scaling;

      -- for padding & detect End_Of_Data_Unit

      -- FIXME all casts Pos Count <-> SIO Pos Count

      DU_Curr_Ix : Positive_Count := Index(FFile);
      DU_Last : constant Positive_Count := Positive_Count(DU_Pos.Get_DU_Last(FFile.PHDU.Pos));
      DU_Item_Last : Positive_Count;
      Is_Last_Write : Boolean := False;
      Last : Count;
   begin

      -- dont Write beyond end of Data Unit

      if(DU_Curr_Ix > DU_Last ) then null; end if;-- FIXME raise error End-Of-DataUnit$

      DU_Item_Last := DU_Curr_Ix + Item'Length - 1;

      Last := Min(Item'Last, 1 + DU_Item_Last - DU_Curr_Ix);

      Is_Last_Write := (DU_Item_Last >= DU_Last);


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
         Loc_Item : T_Arr := Item(Item'First .. Last);
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

      if(Is_Last_Write)
      then
         File.Misc.Write_Padding(FFile.SIO_File,
         SIO.Index(FFile.SIO_File), File.Misc.DataPadValue);
         DU_Pos.Set_DU_Padding_Written(FFile.PHDU.Pos,True);
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

