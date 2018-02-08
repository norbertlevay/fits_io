
with Ada.Text_IO,
     FITS,
     FITS.File,
     System;

use
     System,
     FITS,
     FITS.File;


with PNG_IO;
use  PNG_IO;

with Interfaces;
use  Interfaces;
-- for PutFITSData debug only


package body Commands.PNG is

 subtype pixval is Integer range 0 .. 127;
 type My_Image_Handle is
   array (Natural range <>, Natural range <>) of pixval;

  -- convert Int8 to PNG 8bit
 procedure Int8_To_PNGType( data : in out Int8Arr_Type;
                               -- FIXME 'in' should be enough No 'in out'
                               im   : out My_Image_Handle;
                               W    : in  Natural)
                               -- Width: scanline length
 is
  wi    : Natural := 0;
  hi    : Natural := 0;
 begin

  for dd of data
  loop
    im(hi,wi) := Integer(Abs(dd))/2;
                 -- FIXME why needed /2?
    if wi = W-1 then
     hi := hi + 1;
     wi := 0;
    else
     wi := wi + 1;
    end if;
  end loop;

 end Int8_To_PNGType;

  -- convert Float32 to PNG 8bit
 procedure Float32_To_PNGType( data : in out Float32Arr_Type;
                               -- FIXME 'in' should be enough No 'in out'
                               im   : out My_Image_Handle;
                               W    : in  Natural)
                               -- Width: scanline length
 is
   wi    : Natural := 0;
   hi    : Natural := 0;
 begin

  -- reverse byte order if system is LittleEndian
  -- FITS standard requires BigEndian IEEE Float32
  if System.Default_Bit_Order = System.LOW_ORDER_FIRST
  then
    Endianness_Float32(data);
  end if;

  for dd of data
   loop
     if dd >= 0.0 and dd <= 127.0 then
       im(hi,wi) := Natural(dd);
     elsif dd < 0.0 then
       im(hi,wi) := Natural(0.0);
     else
       im(hi,wi) := Natural(127.0);
     end if;

     if wi = W-1 then
      hi := hi + 1;
      wi := 0;
     else
      wi := wi + 1;
     end if;
   end loop;

 end Float32_To_PNGType;


 -- convert FITS to PNG image
 -- how to handle more then 2D files ?
 procedure FITS_To_PNG (FitsFileName : in String;
                        PngFileName  : String;
                        HDUNum       : Positive := 1)
 is
  FitsFile : SIO.File_Type;
  HDUSize  : HDU_Size_Type;
 begin

  -- -----------------
  -- read FITS file:
  --
  SIO.Open(FitsFile,SIO.In_File,FitsFileName);

  Parse_HeaderBlocks(FitsFile,HDUSize);
   -- move behind the Header

  declare
     dt : FitsData_Type := To_FitsDataType(HDUSize.DUSizeKeyVals.BITPIX);
     W  : constant Dimension    := Integer(HDUSize.DUSizeKeyVals.NAXISn(2));
     H  : constant Dimension    := Integer(HDUSize.DUSizeKeyVals.NAXISn(1));
                                  -- FIXME explicit cast!
     DataD : DataArray_Type( dt, W*H );
        -- holds data from FITS-file
     F : My_Image_Handle(0..(H-1), 0..(W-1));-- := (others => 127);
        -- holds data for PNG image write
     wi    : Natural := 0;
     hi    : Natural := 0;
  begin
     Ada.Text_IO.Put_Line("DU type: " & FitsData_Type'Image(dt));
     Ada.Text_IO.Put     (Integer'Image(W) & " x " );
     Ada.Text_IO.Put_Line(Integer'Image(H) );

     DataArray_Type'Read (SIO.Stream(FitsFile), DataD);

     if dt = Float32 then

        Float32_To_PNGType( DataD.Float32Arr, F, W);

     elsif dt = Int8 then

        Int8_To_PNGType( DataD.Int8Arr, F, W);

     else

       Ada.Text_IO.Put_Line("Not implemented for " & FitsData_Type'Image(dt));

     end if;

   -- -----------------
   -- write PNG file:
   --
   -- requires instantiation of generic Write_PNG_Type_0() func from PNG_IO.ads
   -- needs 3 things:
   --   something what holds the pixels -> Image_Handle - below not used
   --   type of one pixel/Sample -> below Natural
   --   written function which returns each pixel by coordinates from the Image_Handle
   -- Read the long comment in: png_io.ads l.367 before generic decl of Write_PNG_Type_0
   declare

    function My_Grey_Sample(I    : My_Image_Handle;
                            R, C : Coordinate) return Natural is
      begin
--       Ada.Text_IO.Put_Line(Coordinate'Image(R) & " x " & Coordinate'Image(C));
       return I(R,C);
      end My_Grey_Sample;

    procedure Write_0 is new Write_PNG_Type_0(My_Image_Handle, Natural, My_Grey_Sample);

   begin
    Write_0(PngFileName, F, H, W); --, D, I, L); Last 3 params have defaults
   end;
   -- END write PNG file
   -- ------------------

   SIO.Close(FitsFile);

  end; -- 1st declare

 end FITS_To_PNG;

end Commands.PNG;

