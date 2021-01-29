

with Ada.Streams.Stream_IO;
with Ada.Exceptions; use Ada.Exceptions;

-- for HDU_Type / Stream
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
use FITS_IO.V3_Types_For_DU;

with File.Misc; -- DataPadding needed

with DU_Pos;

with FITS; use FITS;
with Ada.Text_IO; -- for debug

package body HDU is

   package TIO renames Ada.Text_IO;

   -----------
   -- Utils --
   -----------


   function Data_Element_Count(NAXISn : NAXIS_Array) return Count -- alg
   is
      Data_Cnt : Count := 1;
      use FITS;
   begin
      for I in NAXISn'Range
      loop
         Data_Cnt := Data_Cnt * NAXISn(I);
      end loop;
      return Data_Cnt;
   end Data_Element_Count;


   ----------------------
   -- Media Management --
   ----------------------

   procedure Reset
      (AHDU : in out HDU_Type;
      SIO_HDU_First : SIO.Positive_Count)
   is
   begin
      AHDU.SIO_HDU_First := SIO_HDU_First;
      AHDU.Pos       := Null_Pos_Rec;
      AHDU.Scaling   := Null_Access_Rec;
      AHDU.Cache     := Null_Cache_Rec;
   end Reset;


   procedure Write_Data_Unit_Padding(SIO_F : SIO.File_Type)
   is
   begin
      File.Misc.Write_Padding(SIO_F, SIO.Index(SIO_F), File.Misc.DataPadValue);
   end Write_Data_Unit_Padding;



   function End_Of_Data_Unit (File : HDU_Type) return Boolean
   is
   begin
      -- FIXME implement: return HDU.DU_Length > SIO.Index(current)
      return True;--SIO.End_Of_File(File.SIO_File);
   end End_Of_Data_Unit;

   function Stream (File : HDU_Type) return SIO.Stream_Access
   is
   begin
      -- FIXME this is the point!
      -- HDU Stream must have all of the properties in HDU_Type
      return SIO.Stream(File.SIO_File);
   end Stream;


   -----------------------------
   -- Input-Output Operations --
   -----------------------------


   -- API but later hide behind Open
   function  Read_Header
      (SIO_File : SIO.File_Type;
      FFile   : in out HDU_Type;
      Keys     : BS_8_Array)  return Image_Rec
   is
      Mand : Mandatory.Result_Rec := Header.Read_Mandatory(SIO_File);
      -- FIXME check HDU_Type -> raise exception if not the expected type
   begin

      -- store begining of DU for DU Read/Write DU_End-guard and padding write

      DU_Pos.Set_DU_Length( FFile.Pos, Data_Element_Count(Mand.NAXISn) );
      -- FIXME cast


      SIO.Set_Index(SIO_File, FFile.SIO_HDU_First);
      -- FIXME update parser to avoid 2 reads

      declare
         Cards : String_80_Array := Header.Read_Optional(SIO_File, Keys);
         Image : Image_Rec(Mand.NAXIS_Last, Cards'Length);
      begin
         Image.Data_Type   := DU_Types.BITPIX_To_DU_Type(Mand.BITPIX);
         Image.NAXISn      := Mand.NAXISn;
         Image.Image_Cards := Cards;

         -- cache DU-access data

         FFile.Cache.BITPIX := Mand.BITPIX;

         Cache.Parse_Image_Cards
            (FFile.Cache,
            FITS.String_80_Array(Cards)); -- FIXME conversion!

         -- init data unit Access_Rec
         Load_BITPIX_And_Scaling_AB(FFile.Scaling, FFile.Cache);
         Load_Undef_Vals_At_Read   (FFile.Scaling, FFile.Cache);

         return Image;
      end;

   end Read_Header;


   -- API on level of Open which calls Set_Index(HDUFirst)
   function  Read_Cards
      (SIO_File : SIO.File_Type;
      FFile : in out HDU_Type;
      Keys   : BS_8_Array)
      return  String_80_Array
   is
   begin

      SIO.Set_Index(SIO_File, FFile.SIO_HDU_First);

      declare
         Image : Image_Rec := Read_Header(SIO_File, FFile, Keys);
      begin
         return Image.Image_Cards;
      end;

   end Read_Cards;


   -- Write Header


   procedure Write_End
      (SIO_File : SIO.File_Type;
      FFile : in out HDU_Type)
   is
   begin

      -- origpos      FFile.ENDCard_Pos := SIO.Index(FFile.SIO_File);
      DU_Pos.Set_ENDCard_Pos(FFile.Pos, SIO.Index(FFile.SIO_File));

      Header.Write_ENDCard_With_Padding(SIO_File);

      -- origpos      FFile.DU_First := SIO.Index(FFile.SIO_File);
      DU_Pos.Set_DU_First(FFile.Pos, SIO.Index(SIO_File), FFile.Cache.BITPIX);
      -- FIXME note had to use Cache.BITPIX !?

      -- init data unit Access_Rec

      Load_BITPIX_And_Scaling_AB(FFile.Scaling, FFile.Cache);
      Load_Undef_Vals_At_Write  (FFile.Scaling, FFile.Cache);

   end Write_End;


   procedure Write_Card_Arr
      (SIO_File : SIO.File_Type;
      File : in out HDU_Type;
      Item : String_80_Array)
   is
   begin
      String_80_Array'Write(SIO.Stream(SIO_File), Item);
      Cache.Parse_Image_Cards
         (File.Cache,
         FITS.String_80_Array(Item)); -- FIXME conversion!
   end Write_Card_Arr;


  procedure Write_Image
      (SIO_File : SIO.File_Type;
      File       : in out HDU_Type;
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
            Write_Card_Arr(SIO_File, File, Optional_Cards_Prim);
         else
            Write_Card_Arr(SIO_File, File, Optional_Cards_Ext);
         end if;

         Write_Card_Arr(SIO_File, File, Optional_Cards);

         -- cache DU-access data

         File.Cache.BITPIX := BITPIX;
         File.Cache.Aui := Aui;

         DU_Pos.Set_DU_Length(File.Pos, Data_Element_Count(NAXISn));
         -- FIXME needed here? & cast!!

      end;

   end Write_Image;


   -- API later hide behind Create / Open(Out_Mode)
   procedure Write_Header_Prim -- Compose_Header
      (SIO_File : SIO.File_Type;
      File       : in out HDU_Type;
      Raw_Type    : DU_Type;
      NAXISn      : NAXIS_Array;
      Optional_Cards : String_80_Array)
   is
      use Ada.Streams.Stream_IO;
      Is_Primary : Boolean := (File.SIO_HDU_First = 1);
      Prim_First_Card : String_80_Array := Elements.Create_Card_SIMPLE(True);
   begin
      Write_Card_Arr(SIO_File,File, Prim_First_Card);
      Write_Image(SIO_File,File, Raw_Type, NAXISn, Optional_Cards, Is_Primary);
      Write_End(SIO_File, File);
   end Write_Header_Prim;


   -- API later hide behind Open(Append_Mode)
   procedure Write_Header_Ext -- Compose_Header
      (SIO_File : SIO.File_Type;
      File       : in out HDU_Type;
      Raw_Type    : DU_Type;
      NAXISn      : NAXIS_Array;
      Optional_Cards : String_80_Array)
   is
      use Ada.Streams.Stream_IO;
      Is_Primary : Boolean := (File.SIO_HDU_First = 1);
      Ext_First_Card  : String_80_Array := Elements.Create_Card_XTENSION("'IMAGE   '");
   begin
      Write_Card_Arr(SIO_File, File, Ext_First_Card);
      Write_Image(SIO_File, File, Raw_Type, NAXISn, Optional_Cards, Is_Primary);
      Write_End(SIO_File, File);
   end Write_Header_Ext;



   -- API on level od Open/Create; later rename to Append_Cards
   procedure Write_Cards
      (SIO_File : SIO.File_Type;
      File       : in out HDU_Type;
      Cards : String_80_Array)
   is
      Ix : SIO.Positive_Count := 1;
   begin
      -- position File-Index at END-card
      SIO.Set_Index(SIO_File, DU_Pos.Get_ENDCard_Pos(File.Pos));
      Write_Card_Arr(SIO_File, File, Cards);  -- start writing Cards (overwrites existing ENDCard)
      Write_End(SIO_File, File);              -- writes new END-card and padding
   end Write_Cards;




   -- Data


   ----------------------------------------
   -- Operations on Position within File --
   ----------------------------------------

   function Data_Unit_Size  (File : HDU_Type) return Count
   is
   begin
      return File.Pos.DU_Length;
   end Data_Unit_Size;


   -- FITS specific

   ----------------------------------------------
   -- Converions, Scaling and Undefined Values --
   ----------------------------------------------

   procedure Set_Raw_Type(File : in out HDU_Type; Raw_Type : DU_Type)
   is
      BITPIX   : Integer;
      Aui      : Float;
   begin
      DU_Types.DU_Type_To_BITPIX(Raw_Type, BITPIX, Aui);
      File.Cache.BITPIX := BITPIX;
      File.Cache.Aui    := Aui;
   end Set_Raw_Type;


   procedure Set_Linear_Scaling(File : in out HDU_Type; A,B : Float)
   is
   begin
      File.Cache.Au := A;
      File.Cache.Bu := B;
   end Set_Linear_Scaling;


   procedure Set_Undefined_Physical(File : in out HDU_Type; Undef_Phys : Float)
   is
   begin
      File.Cache.Physical_Undef_Valid := True;
      File.Cache.Physical_Undef_Value := Undef_Phys;
   end Set_Undefined_Physical;


   procedure Put_HDU_Type(File : HDU_Type; Prefix : String := "")
   is
   begin
      TIO.Put_Line(Prefix & "Cache Aui = " & Float'Image(File.Cache.Aui));
      Put_Access_Rec(File.Scaling,Prefix);
   end Put_HDU_Type;


   ----------------------
   -- Data Unit access --
   ----------------------

   -- Random access Index : 1 .. DU_Last

   -- FIXME stop compile on system where DE_Site SE_Size is not divisible
   -- or how to avoid DE/SE & SE/DE divisions ?

   function  Index(File : HDU_Type) return Positive_Count
   is  
      SIO_Index  : SIO.Positive_Count := SIO.Index(File.SIO_File);
   begin
      return Positive_Count(
         DU_Pos.DU_Index(SIO_Index, File.Pos.SIO_DU_First, File.Scaling.BITPIX) );
   end Index;


   procedure Set_Index(File : HDU_Type; Ix : Positive_Count)
   is  
      SIO_Index : SIO.Positive_Count;
      use SIO;
      HDU_Inited : Boolean := (File.Pos.SIO_DU_First /= 0) AND (File.Scaling.BITPIX /= 0); 
   begin
      if(HDU_Inited)
      then
         SIO_Index := DU_Pos.SE_Index(Ix,
                                      File.Pos.SIO_DU_First, File.Scaling.BITPIX);
         SIO.Set_Index(File.SIO_File, SIO_Index);
      else
         null; -- FIXME programming error: Set_Index called but HDU is empty
      end if;
   end Set_Index;




   function Min(A,B : Count) return Count
   is
      use FITS;
   begin
      if (A > B) then return B; else return A; end if;
   end Min;


   -- Data access

   procedure HDU_Read
      (FFile    : in out HDU_Type;
      Item : out T_Arr;
      Last : out Count)
   is
      type Float_Arr is array (FITS.Positive_Count range <>) of Float;
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

      DU_Curr_Ix : Positive_Count := Index(FFile);
      DU_Last : constant Positive_Count := Positive_Count(DU_Pos.Get_DU_Last(FFile.Pos));
      DU_Item_Last : Positive_Count;
      use FITS;
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
      (FFile : in out HDU_Type;
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

      -- FIXME all casts Pos Count <-> SIO Pos Count

      DU_Curr_Ix : Positive_Count := Index(FFile);
      DU_Last : constant Positive_Count := Positive_Count(DU_Pos.Get_DU_Last(FFile.Pos));
      DU_Item_Last : Positive_Count;
      Is_Last_Write : Boolean := False;
      Last : Count;
      use FITS;
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
         DU_Pos.Set_DU_Padding_Written(FFile.Pos,True);
      end if;

   end HDU_Write;


end HDU;

