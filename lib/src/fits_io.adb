

with Mandatory; -- Result_rec needed
with Header;    -- Valued_Key_Record_Arr needed


package body FITS_IO is

   -- Image_Data_Model conversions


   -- for Read direction

   function To_Physical
      (File_Image : Image_Data_Model;
      Mem_BITPIX : Integer;
      Mem_Undef  : BS70.Bounded_String)-- caller's preference for Undef
      return Image_Data_Model
   is
      Phys_Im : Image_Data_Model := File_Image;
      -- NAXISn and Unit copied, will not change
      use BS70;-- operator '=' needed
   begin

      Phys_Im.BITPIX := Mem_BITPIX;
      -- FIXME type conversion: Tmem must be known:

      -- read from BZERO BSCALE
      Phys_Im.A := -File_Image.A / File_Image.B;
      Phys_Im.B :=  1.0 / File_Image.B;

      -- read from BLANK
      if(File_Image.Undef = Null_Undefined_Value)
      then
         Phys_Im.Undef := Null_Undefined_Value;
      else
          -- if caller supplied Phys.Undef use it, otherwise calculate it
         if(Mem_Undef = Null_Undefined_Value)
         then
            Phys_Im.Undef := Mem_Undef;
         else
            declare
               Uraw : Float := Float'Value(BS70.To_String(File_Image.Undef));
               Uph  : Float := File_Image.A + File_Image.B * Uraw;
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

   function To_Cards(Image : Image_Data_Model) return Header.Valued_Key_Record_Arr
   is
      Key_Recs : Header.Valued_Key_Record_Arr(0..1);
   begin
      -- FIXME implement
      return Key_Recs;
   end To_Cards;



   -- for Read: convert from Mandatory.Result_Rec -> Image_Data_Model

   function To_Image(Parse_Result : Mandatory.Result_Rec) return Image_Data_Model
   is
      Image : Image_Data_Model(Parse_Result.NAXIS_Last);
   begin
      -- FIXME implement
      return Image;
   end To_Image;






   -- API

   procedure Read_Header
     (File  : SIO.File_Type;
      Image : in out Image_Data_Model)
   is
      Parsed_Mandatory  : Mandatory.Result_Rec  := Header.Read_Mandatory(File);
      Image_File        : Image_Data_Model      := To_Image(Parsed_Mandatory);

      Mem_BITPIX : Integer := Image.BITPIX; -- FIXME where to get this ? or needed here ?
      Mem_Undef  : BS70.Bounded_String := Null_Undefined_Value;-- FIXME how to get this ?
   begin
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
      Key_Recs   : Header.Valued_Key_Record_Arr := To_Cards(Image_File);
   begin
      Header.Valued_Key_Record_Arr'Write(SIO.Stream(File), Key_Recs);
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

