
with Ada.Strings.Fixed;
with Ada.Strings; -- Trim needed
use Ada.Strings; -- Left needed for Trim

with Mandatory; -- Result_rec needed
with Header;    -- Valued_Key_Record_Arr needed


package body FITS_IO is


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




   -- Image_Data_Model conversions


   -- for Read direction

   function To_Physical
      (Raw_Image  : Image_Data_Model;
      Phys_BITPIX : Integer;
      Phys_Undef  : BS70.Bounded_String)-- caller's preference for Undef
      return Image_Data_Model
   is
      Phys_Im : Image_Data_Model := Raw_Image;
      -- NAXISn and Unit copied, will not change
      use BS70;-- operator '=' needed
   begin

      Phys_Im.BITPIX := Phys_BITPIX;
      -- FIXME type conversion: Tmem must be known:

      -- read from BZERO BSCALE
      Phys_Im.A := -Raw_Image.A / Raw_Image.B;
      Phys_Im.B :=  1.0 / Raw_Image.B;

      -- read from BLANK
      if(Raw_Image.Undef = Null_Undefined_Value)
      then
         Phys_Im.Undef := Null_Undefined_Value;
      else
          -- if caller supplied Phys.Undef use it, otherwise calculate it
         if(Phys_Undef = Null_Undefined_Value)
         then
            Phys_Im.Undef := Phys_Undef;
         else
            declare
               Uraw : Float := Float'Value(BS70.To_String(Raw_Image.Undef));
               Uph  : Float := Raw_Image.A + Raw_Image.B * Uraw;
            begin
               Phys_Im.Undef := BS70.To_Bounded_String(Float'Image(Uph));
            end;
        end if;
      end if;

      return Phys_Im;
   end To_Physical;



   -- for Write direction

   function To_Raw
      (Physical_Image : Image_Data_Model;
      Raw_BITPIX : Integer;
      Raw_Undef  : BS70.Bounded_String) -- caller's preference for Undef
      return Image_Data_Model
   is
      File_Im : Image_Data_Model := Physical_Image;
      -- copy NAXISn and Unit - those do not change
      use BS70;-- operator '=' needed
   begin

      File_Im.BITPIX := Raw_BITPIX;--Physical_Image.BITPIX;
      -- FIXME for starters no conversion; write in same type as available data

      -- later write as BZERO BSCALE
      File_Im.A := -Physical_Image.A / Physical_Image.B;
      File_Im.B := 1.0 / Physical_Image.B;

      -- later write as BLANK
      if (Physical_Image.Undef = Null_Undefined_Value)
      then
         File_Im.Undef := Null_Undefined_Value;
      else
         -- if caller supplied File.Undef use it, otherwise calculate it
         if(Raw_Undef = Null_Undefined_Value)
         then
            File_Im.Undef := Raw_Undef;
         else
            declare
               Uph  : Float := Float'Value(BS70.To_String(Physical_Image.Undef));
               Uraw : Float := Physical_Image.A + Physical_Image.B * Uph;
            begin
               File_Im.Undef := BS70.To_Bounded_String(Float'Image(Uraw));
            end;
         end if;
      end if;

      return File_Im;
   end To_Raw;





   -- for Write: convert from Image_Data_Model -> Cards

   function Needed_Array_Length(Image : Image_Data_Model) return Natural
   is
      Len : Natural;
      use BS70;
   begin
      -- BITPIX + NAXIS + NAXIS array
      Len := 2 + Image.NAXISn'Length;
      -- optional BLANK
      if(Image.Undef /= Null_Undefined_Value)
      then
         Len := Len + 1;
      end if;
      -- optional BUNIT
      if(Image.Unit /= Null_Undefined_Value)
      then
         Len := Len + 1;
      end if;
      -- optional BZERO BSCALE
      if(True)--Image.BITPIX >= 0) -- if not Float
      then
         Len := Len + 2;
      end if;
      return Len;
   end Needed_Array_Length;


   function To_Cards(Image : Image_Data_Model) return Header.Valued_Key_Record_Arr
   is
      Card_Count  : Natural := Needed_Array_Length(Image);
      Key_Recs    : Header.Valued_Key_Record_Arr(1 .. Card_Count);
      use BS_8;
      use BS70;
      use Ada.Strings.Fixed;--Trim needed
      Bx : Integer;
   begin
      -- FIXME implement converion function CardKey,CardValue <-> Ada/FITSlib native types
      -- BITPIX NAXIS Undef Unit A B
      Key_Recs(1) := (1* "BITPIX", 1*Integer'Image(Image.BITPIX));
      Key_Recs(2) := (1* "NAXIS",  1*NAXIS_Index'Image(Image.NAXISn'Last));
      for I in Image.NAXISn'Range
      loop
         Key_Recs(3 + (I - 1))
           := (1*("NAXIS" & Trim(Integer'Image(I),Left)),1*Positive_Count'Image(Image.NAXISn(I)));
      end loop;
      -- base index: points to next free card-slot
      Bx := 3 + (Image.NAXISn'Length);
       -- optional BLANK
      if(Image.Undef /= Null_Undefined_Value)
      then
         Key_Recs(Bx) := (1*"BLANK", Image.Undef);
         Bx := Bx + 1;
      end if;
      -- optional BUNIT
      if(Image.Unit /= Null_Undefined_Value)
      then
         Key_Recs(Bx) := (1*"BUNIT", Image.Unit);
         Bx := Bx + 1;
      end if;
      -- optional BZERO BSCALE
      if(True)--Image.BITPIX >= 0) -- if not Float
      then
         Key_Recs(Bx) := (1*"BZERO",  1* Float'Image(Image.A));
         Bx := Bx + 1;
         Key_Recs(Bx) := (1*"BSCALE", 1* Float'image(Image.B));
         Bx := Bx + 1;
      end if;
      return Key_Recs;
   end To_Cards;



   -- for Read: convert from Mandatory.Result_Rec -> Image_Data_Model

   function To_Image(Parse_Result : Mandatory.Result_Rec) return Image_Data_Model
   is
      Image : Image_Data_Model(Parse_Result.NAXIS_Last);
   begin
      Image.BITPIX := Parse_Result.BITPIX;
      Image.NAXISn := Parse_Result.NAXISn;
      -- FIXME implement missing parse Optional: Undef Unit A B
      -- now set to default
      Image.Undef := Null_Undefined_Value;
      Image.Unit  := Null_Undefined_Value;
      Image.A := 0.0;
      Image.B := 1.0;
      return Image;
   end To_Image;


   -- low-level API

   procedure Read_Raw_Header
     (File  : SIO.File_Type;
      Raw_Image : in out Image_Data_Model) -- FIXME review why in-out arg?
   is
      Parsed_Mandatory  : Mandatory.Result_Rec  := Header.Read_Mandatory(File);
   begin
      Raw_Image := To_Image(Parsed_Mandatory);
   end Read_Raw_Header;
   -- FIXME verify: call must leave File Index pointing to DataUnit



   procedure Write_Raw_Header
      (File  : SIO.File_Type;
       Raw_Image : Image_Data_Model)
   is
      Key_Recs   : Header.Valued_Key_Record_Arr := To_Cards(Raw_Image);
   begin
      Header.Valued_Key_Record_Arr'Write(SIO.Stream(File), Key_Recs);
   end Write_Raw_Header;
   -- FIXME Write END-card and padding
   -- FIXME verify: call must leave File Index pointing to DataUnit






   -- high-level API

   procedure Read_Header
     (File  : SIO.File_Type;
      Image : in out Image_Data_Model)
   is
--      Parsed_Mandatory  : Mandatory.Result_Rec  := Header.Read_Mandatory(File);
      Image_File        : Image_Data_Model(Image.NAXISn'Last);-- := To_Image(Parsed_Mandatory);

      Mem_BITPIX : Integer := Image.BITPIX; -- FIXME where to get this ? or needed here ?
      Mem_Undef  : BS70.Bounded_String := Null_Undefined_Value;-- FIXME how to get this ?
   begin
      Read_Raw_Header(File, Image_File);
      Image := To_Physical(Image_File, Mem_BITPIX, Mem_Undef);
   end Read_Header;
   -- FIXME verify: call must leave File Index pointing to DataUnit



   procedure Write_Header
      (File  : SIO.File_Type;
       Image : Image_Data_Model)
   is
      Raw_BITPIX : Integer := Image.BITPIX; -- FIXME where to get this ? or needed here ?
      Raw_Undef  : BS70.Bounded_String := Null_Undefined_Value;-- FIXME how to get this ?

      Image_File : Image_Data_Model             := To_Raw(Image, Raw_BITPIX, Raw_Undef);
--      Key_Recs   : Header.Valued_Key_Record_Arr := To_Cards(Image_File);
   begin
--      Header.Valued_Key_Record_Arr'Write(SIO.Stream(File), Key_Recs);
      Write_Raw_Header(File, Image_File);
   end Write_Header;
   -- FIXME Write END-card and padding
   -- FIXME verify: call must leave File Index pointing to DataUnit




--   procedure Read_Header
--     (File    : SIO.File_Type;
--      Scaling : out Scaling_Rec;
--      NAXISn : out NAXIS_Array;
--      Undef  : in out BS70.Bounded_String) is begin null; end;

--   procedure Write_Header
--     (File    : SIO.File_Type;
--       Scaling : Scaling_Rec;
--       NAXISn : NAXIS_Array;
--       Undef  : BS70.Bounded_String := Null_Undefined_Value) is begin null; end;

end FITS_IO;

