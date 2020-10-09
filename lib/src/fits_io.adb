

with Mandatory; -- Result_rec needed
with Header;    -- Valued_Key_Record_Arr needed


package body FITS_IO is

   -- Image_Data_Model conversions





   -- for Read direction
   function To_Physical
      (File_Image : Image_Data_Model;
      Mem_BITPIX : Integer) return Image_Data_Model
   is
      Phys_Im : Image_Data_Model := File_Image;
      -- NAXISn and Unit copied, will not change
   begin
      Phys_Im.BITPIX := Mem_BITPIX; -- FIXME type conversion: Tmem must be known:
      Phys_Im.Undef  := Null_Undefined_Value;-- convert Undef_Tf=BLANK -> Undef_Tm, if BLANK exist
      Phys_Im.A := 0.0; -- = BZERO
      Phys_Im.B := 1.0; -- = BSCALE
      return Phys_Im;
   end To_Physical;





   -- for Write direction
   function To_Raw
      (Physical_Image : Image_Data_Model;
      Mem_BITPIX : Integer) return Image_Data_Model
   is
      File_Im : Image_Data_Model := Physical_Image;
      -- copy NAXISn and Unit - those do not change
   begin
      File_Im.BITPIX := Mem_BITPIX; -- no conversion; write in same type as available data
      File_Im.Undef := Null_Undefined_Value; -- convert Undef_Tm -> Undef_Tf if U_Tm exists
      File_Im.A := 0.0; -- such that if Tmem Values scaled, results are in [Units]
      File_Im.B := 1.0; -- e.g. Vout [Unit] = A + B * Vmem   [BZERO,BSCALE] = Inverz([A,B])
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
      Image : out Image_Data_Model)
   is
      Parsed_Mandatory  : Mandatory.Result_Rec := Header.Read_Mandatory(File);
      Image_File        : Image_Data_Model := To_Image(Parsed_Mandatory);
      Mem_BITPIX        : Integer := 0;-- FIXME where to get this ?
   begin
      Image := To_Physical(Image_File, Mem_BITPIX);
   end Read_Header;
   -- FIXME verify: call must leave File Index pointing to DataUnit



   procedure Write_Header
      (File  : SIO.File_Type;
       Image : Image_Data_Model)
   is
      Mem_BITPIX : Integer := 0; -- FIXME where to get this ?
      Image_File : Image_Data_Model := To_Raw(Image, Mem_BITPIX);
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

