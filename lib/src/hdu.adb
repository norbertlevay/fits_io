

with Ada.Streams.Stream_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Streams; use Ada.Streams;
with System;

-- for HDU_Type / Stream
with Header; -- Read Mandatory / Optional needed
with Cache;

-- for Header Parse/Compose
with DU_Types;
with Mandatory; -- Result_Rec needed
with Elements;

-- for Data Unit Read/Write
with Numeric_Type;
with Numeric_Type.Data_IO;
with V3_Types; use V3_Types;
with Value;

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


   procedure Write_Data_Unit_Padding(SIO_File : SIO.File_Type)
   is
   begin
      File.Misc.Write_Padding(SIO_File, SIO.Index(SIO_File), File.Misc.DataPadValue);
   end Write_Data_Unit_Padding;



   function End_Of_Data_Unit (AHDU : HDU_Type) return Boolean
   is
   begin
      -- FIXME implement: return HDU.Pos.DU_Length > SIO.Index(current)
      return True;--SIO.End_Of_File(File.SIO_File);
   end End_Of_Data_Unit;


   -----------------------------
   -- Input-Output Operations --
   -----------------------------


   -- API but later hide behind Open
   function  Read_Header
      (SIO_File : SIO.File_Type;
      AHDU   : in out HDU_Type;
      Keys     : BS_8_Array)  return Image_Rec
   is
      Mand : Mandatory.Result_Rec := Header.Read_Mandatory(SIO_File);
      -- FIXME check HDU_Type -> raise exception if not the expected type
   begin

      -- store begining of DU for DU Read/Write DU_End-guard and padding write

      DU_Pos.Set_DU_First(AHDU.Pos, SIO.Index(SIO_File), AHDU.Cache.BITPIX);
      DU_Pos.Set_DU_Length( AHDU.Pos, Data_Element_Count(Mand.NAXISn) );
      -- FIXME cast


      SIO.Set_Index(SIO_File, AHDU.SIO_HDU_First);
      -- FIXME update parser to avoid 2 reads

      declare
         Cards : String_80_Array := Header.Read_Optional(SIO_File, Keys);
         Image : Image_Rec(Mand.NAXIS_Last, Cards'Length);
      begin
         Image.Data_Type   := DU_Types.BITPIX_To_DU_Type(Mand.BITPIX);
         Image.NAXISn      := Mand.NAXISn;
         Image.Image_Cards := Cards;

         -- cache DU-access data

         AHDU.Cache.BITPIX := Mand.BITPIX;

         Cache.Parse_Image_Cards
            (AHDU.Cache,
            FITS.String_80_Array(Cards)); -- FIXME conversion!

         -- init data unit Access_Rec
         Load_BITPIX_And_Scaling_AB(AHDU.Scaling, AHDU.Cache);
         Load_Undef_Vals_At_Read   (AHDU.Scaling, AHDU.Cache);

         return Image;
      end;

   end Read_Header;


   -- API on level of Open which calls Set_Index(HDUFirst)
   function  Read_Cards
      (SIO_File : SIO.File_Type;
      AHDU : in out HDU_Type;
      Keys   : BS_8_Array)
      return  String_80_Array
   is
   begin

      SIO.Set_Index(SIO_File, AHDU.SIO_HDU_First);

      declare
         Image : Image_Rec := Read_Header(SIO_File, AHDU, Keys);
      begin

         DU_Pos.Set_DU_First(AHDU.Pos, SIO.Index(SIO_File), AHDU.Cache.BITPIX);

         return Image.Image_Cards;
      end;

   end Read_Cards;


   -- Write Header


   procedure Write_End
      (SIO_File : SIO.File_Type;
      AHDU : in out HDU_Type)
   is
   begin

      -- origpos      AHDU.ENDCard_Pos := SIO.Index(AHDU.SIO_File);
      DU_Pos.Set_ENDCard_Pos(AHDU.Pos, SIO.Index(SIO_File));

      Header.Write_ENDCard_With_Padding(SIO_File);

      -- origpos      AHDU.DU_First := SIO.Index(AHDU.SIO_File);
      DU_Pos.Set_DU_First(AHDU.Pos, SIO.Index(SIO_File), AHDU.Cache.BITPIX);
      -- FIXME note had to use Cache.BITPIX !?

      -- init data unit Access_Rec

      Load_BITPIX_And_Scaling_AB(AHDU.Scaling, AHDU.Cache);
      Load_Undef_Vals_At_Write  (AHDU.Scaling, AHDU.Cache);

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
      AHDU       : in out HDU_Type;
      Raw_Type    : DU_Type;
      NAXISn      : NAXIS_Array;
      Optional_Cards : String_80_Array)
   is
      use Ada.Streams.Stream_IO;
      Is_Primary : Boolean := (AHDU.SIO_HDU_First = 1);
      Prim_First_Card : String_80_Array := Elements.Create_Card_SIMPLE(True);
   begin
      Write_Card_Arr(SIO_File,AHDU, Prim_First_Card);
      Write_Image(SIO_File,AHDU, Raw_Type, NAXISn, Optional_Cards, Is_Primary);
      Write_End(SIO_File, AHDU);
   end Write_Header_Prim;


   -- API later hide behind Open(Append_Mode)
   procedure Write_Header_Ext -- Compose_Header
      (SIO_File : SIO.File_Type;
      AHDU       : in out HDU_Type;
      Raw_Type    : DU_Type;
      NAXISn      : NAXIS_Array;
      Optional_Cards : String_80_Array)
   is
      use Ada.Streams.Stream_IO;
      Is_Primary : Boolean := (AHDU.SIO_HDU_First = 1);
      Ext_First_Card  : String_80_Array := Elements.Create_Card_XTENSION("'IMAGE   '");
   begin
      Write_Card_Arr(SIO_File, AHDU, Ext_First_Card);
      Write_Image(SIO_File, AHDU, Raw_Type, NAXISn, Optional_Cards, Is_Primary);
      Write_End(SIO_File, AHDU);
   end Write_Header_Ext;



   -- API on level od Open/Create; later rename to Append_Cards
   procedure Write_Cards
      (SIO_File : SIO.File_Type;
      AHDU       : in out HDU_Type;
      Cards : String_80_Array)
   is
      Ix : SIO.Positive_Count := 1;
   begin
      -- position File-Index at END-card
      SIO.Set_Index(SIO_File, DU_Pos.Get_ENDCard_Pos(AHDU.Pos));
      Write_Card_Arr(SIO_File, AHDU, Cards);  -- start writing Cards (overwrites existing ENDCard)
      Write_End(SIO_File, AHDU);              -- writes new END-card and padding
   end Write_Cards;




   -- Data


   ----------------------------------------
   -- Operations on Position within File --
   ----------------------------------------

   function Data_Unit_Size  (AHDU : HDU_Type) return Count
   is
   begin
      return AHDU.Pos.DU_Length;
   end Data_Unit_Size;


   -- FITS specific

   ----------------------------------------------
   -- Converions, Scaling and Undefined Values --
   ----------------------------------------------

   procedure Set_Raw_Type(AHDU : in out HDU_Type; Raw_Type : DU_Type)
   is
      BITPIX   : Integer;
      Aui      : Float;
   begin
      DU_Types.DU_Type_To_BITPIX(Raw_Type, BITPIX, Aui);
      AHDU.Cache.BITPIX := BITPIX;
      AHDU.Cache.Aui    := Aui;
   end Set_Raw_Type;


   procedure Set_Linear_Scaling(AHDU : in out HDU_Type; A,B : Float)
   is
   begin
      AHDU.Cache.Au := A;
      AHDU.Cache.Bu := B;
   end Set_Linear_Scaling;


   procedure Set_Undefined_Physical(AHDU : in out HDU_Type; Undef_Phys : Float)
   is
   begin
      AHDU.Cache.Physical_Undef_Valid := True;
      AHDU.Cache.Physical_Undef_Value := Undef_Phys;
   end Set_Undefined_Physical;


   procedure Put_HDU_Type(AHDU : HDU_Type; Prefix : String := "")
   is
   begin
      TIO.Put_Line(Prefix & "SIO_DU_First = "& SIO.Positive_Count'Image(AHDU.Pos.SIO_DU_First));
      TIO.Put_Line(Prefix & "Cache Aui = " & Float'Image(AHDU.Cache.Aui));
      Put_Access_Rec(AHDU.Scaling,Prefix);
   end Put_HDU_Type;


   ----------------------
   -- Data Unit access --
   ----------------------

   -- Random access Index : 1 .. DU_Last

   -- FIXME stop compile on system where DE_Site SE_Size is not divisible
   -- or how to avoid DE/SE & SE/DE divisions ?

   function  Index(SIO_File: SIO.File_Type; AHDU : HDU_Type) return Positive_Count
   is  
      SIO_Index  : SIO.Positive_Count := SIO.Index(SIO_File);
   begin
      return Positive_Count(
         DU_Pos.DU_Index(SIO_Index, AHDU.Pos.SIO_DU_First, AHDU.Scaling.BITPIX) );
   end Index;


   procedure OFF_Set_Index(SIO_File : SIO.File_Type; AHDU : HDU_Type; Ix : Positive_Count)
   is
      SIO_Index : SIO.Positive_Count;
      use SIO;
      HDU_Inited : Boolean := (AHDU.Pos.SIO_DU_First /= 0) AND (AHDU.Scaling.BITPIX /= 0); 
   begin
      if(HDU_Inited)
      then
         SIO_Index := DU_Pos.SE_Index(Ix,
         AHDU.Pos.SIO_DU_First, AHDU.Scaling.BITPIX);
         SIO.Set_Index(SIO_File, SIO_Index);
      else
         null; -- FIXME programming error: Set_Index called but HDU is empty
      end if;
   end OFF_Set_Index;




   function Min(A,B : Count) return Count
   is
      use FITS;
   begin
      if (A > B) then return B; else return A; end if;
   end Min;


   -- Data access


   type Float_Arr is array (FITS.Positive_Count range <>) of Float;
   package U8Raw  is new Numeric_Type(Unsigned_8, U8_Arr,    Float_Arr);
   package I16Raw  is new Numeric_Type(Integer_16, I16_Arr,    Float_Arr);
   package I32Raw  is new Numeric_Type(Integer_32, I32_Arr,    Float_Arr);
   package I64Raw  is new Numeric_Type(Integer_64, I64_Arr,    Float_Arr);
   package F32Raw  is new Numeric_Type(Float_32,   F32_Arr,    Float_Arr);
   package F64Raw  is new Numeric_Type(Float_64,   F64_Arr,    Float_Arr);

   package F32Raw_DIO is new F32Raw.Data_IO;


   procedure My_Read
      (SIO_File : SIO.File_Type;
      AHDU      : in out HDU_Type;
      Item : out T_Arr;
      Last : out Count)
   is
      package Physical is new Numeric_Type(T, T_Arr, Float_Arr);
      package U8_Value is new Value(U8Raw, Physical);
      package I16_Value is new Value(I16Raw, Physical);
      package I32_Value is new Value(I32Raw, Physical);
      package I64_Value is new Value(I64Raw, Physical);
      package F32_Value is new Value(F32Raw, Physical);
      package F64_Value is new Value(F64Raw, Physical);

      Scaling : Access_Rec := AHDU.Scaling;

      -- for padding & detect End_Of_Data_Unit

      DU_Curr_Ix : Positive_Count := Index(SIO_File, AHDU);
      DU_Last : constant Positive_Count := DU_Pos.Get_DU_Last(AHDU.Pos);
      DU_Item_Last : Positive_Count;
      use FITS;
   begin

      if(DU_Curr_Ix > DU_Last )
      then
         TIO.Put_Line("EXCEPT: End Of Data Unit in HDU_Read");
      end if;

      DU_Item_Last := DU_Curr_Ix + Item'Length - 1;

      Last := Min(Item'Last, 1 + DU_Item_Last - DU_Curr_Ix);

      -- Set Undefined value

      if(False)
      --if(Scaling.Undef_Used)
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
         Loc_Item : T_Arr := Item(Item'First .. Item'First + Last - 1);
         S : access Ada.Streams.Root_Stream_Type'Class := SIO.Stream(SIO_File);
         Length : Ada.Streams.Stream_Element_Offset;--FIXME see Last vs Length
      begin

         -- Scaling

         case(Scaling.BITPIX) is
            when   8 => U8_Value.Read(SIO.Stream(SIO_File), Scaling.A,Scaling.B, Loc_Item);
            when  16 => I16_Value.Read(SIO.Stream(SIO_File), Scaling.A,Scaling.B, Loc_Item);
            when  32 => I32_Value.Read(SIO.Stream(SIO_File), Scaling.A,Scaling.B, Loc_Item);
            when  64 => I64_Value.Read(SIO.Stream(SIO_File), Scaling.A,Scaling.B, Loc_Item);

            when -32 =>
               declare
                  RawArr : F32_Arr(Loc_Item'Range);
               begin
                  F32Raw_DIO.Read_Buffered(SIO_File, RawArr, Length);
                  F32_Value.Raw_To_Phys(RawArr, Scaling, Loc_Item);
               end;

            when -64 => F64_Value.Read(SIO.Stream(SIO_File), Scaling.A,Scaling.B, Loc_Item);

            when others =>
               Raise_Exception(Programming_Error'Identity, "BITPIX: "&Integer'Image(Scaling.BITPIX));
         end case;

         Item(Item'First .. Last) := Loc_Item;

      end;

   end My_Read;



   procedure My_Write
      (SIO_File : SIO.File_Type;
      AHDU      : in out HDU_Type;
      Item : T_Arr)
   is
      package Physical is new Numeric_Type(T, T_Arr, Float_Arr);
      package U8_Value is new Value(U8Raw, Physical);
      package I16_Value is new Value(I16Raw, Physical);
      package I32_Value is new Value(I32Raw, Physical);
      package I64_Value is new Value(I64Raw, Physical);
      package F32_Value is new Value(F32Raw, Physical);
      package F64_Value is new Value(F64Raw, Physical);

      Scaling : Access_Rec := AHDU.Scaling;

      -- for padding & detect End_Of_Data_Unit

      -- FIXME all casts Pos Count <-> SIO Pos Count

      DU_Curr_Ix : Positive_Count := Index(SIO_File, AHDU);
      DU_Last : constant Positive_Count := DU_Pos.Get_DU_Last(AHDU.Pos);
      DU_Item_Last : Positive_Count;
      Is_Last_Write : Boolean := False;
      Last : Count;
   begin

      -- dont Write beyond end of Data Unit

      if(DU_Curr_Ix > DU_Last )
      then
         TIO.Put_Line("EXCEPT: End Of Data Unit in HDU_Write");
      end if;

      DU_Item_Last := DU_Curr_Ix + Item'Length - 1;

      Last := Min(Item'Last, 1 + DU_Item_Last - DU_Curr_Ix);

      Is_Last_Write := (DU_Item_Last >= DU_Last);


      -- Set Undefined value

      if(False)
      --if(Scaling.Undef_Used)
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
         Loc_Item : T_Arr := Item(Item'First .. Item'First + Last - 1);
      begin

         -- Scaling

         case(Scaling.BITPIX) is
            when   8 =>  U8_Value.Write(SIO.Stream(SIO_File), Scaling.A,Scaling.B, Loc_Item);
            when  16 => I16_Value.Write(SIO.Stream(SIO_File), Scaling.A,Scaling.B, Loc_Item);
            when  32 => I32_Value.Write(SIO.Stream(SIO_File), Scaling.A,Scaling.B, Loc_Item);
            when  64 => I64_Value.Write(SIO.Stream(SIO_File), Scaling.A,Scaling.B, Loc_Item);
            when -32 => --F32_Value.Write(SIO.Stream(SIO_File), Scaling.A,Scaling.B, Loc_Item);
               declare
                  RawArr : F32_Arr(Loc_Item'Range);
               begin
                  F32_Value.Phys_To_Raw(RawArr, Scaling, Loc_Item);
                  F32Raw_DIO.Write_Buffered(SIO_File, RawArr);
               end;


            when -64 => F64_Value.Write(SIO.Stream(SIO_File), Scaling.A,Scaling.B, Loc_Item);
            when others =>
               Raise_Exception(Programming_Error'Identity,
               "BITPIX: "&Integer'Image(Scaling.BITPIX));
         end case;

      end;

      -- add padding if wrote last data in Data Unit

      if(Is_Last_Write)
      then
         File.Misc.Write_Padding(SIO_File,
         SIO.Index(SIO_File), File.Misc.DataPadValue);
         DU_Pos.Set_DU_Padding_Written(AHDU.Pos,True);
      end if;

   end My_Write;


end HDU;

