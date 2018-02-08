
with Ada.Text_IO,
     FITS,
     FITS.File,
     System;

use
     FITS,
     FITS.File,
     System;

with PNG_IO;
use  PNG_IO;

with Interfaces;
use  Interfaces;


package body Commands.PNG is

 subtype pixval is Integer range 0 .. 255;
-- subtype pixval is Integer range 0 .. 65535;
 type My_Image_Handle is
   array (Natural range <>, Natural range <>) of pixval;


 procedure Convert_FITS_Int8_To_PNG_Int8
           (Data : in  Int8Arr_Type;    -- FITS data
            Img  : out My_Image_Handle; -- PNG pixels (the image)
            W    : in  Natural)         -- Image/Data width
 is
  wi    : Natural := 0;
  hi    : Natural := 0;
 begin

  for dd of Data
  loop

    Img(wi,hi) := Integer(dd) + 128;
                 -- FIXME explicit conversion
                 -- from Interfaces.Integer_8 -> Standard.Integer
    if wi = W-1 then
     hi := hi + 1;
     wi := 0;
    else
     wi := wi + 1;
    end if;
  end loop;

 end Convert_FITS_Int8_To_PNG_Int8;


 procedure Convert_FITS_Float32_To_PNG_Int8
           (Data : in  Float32Arr_Type; -- FITS data
            Img  : out My_Image_Handle; -- PNG pixels (the image)
            W    : in  Natural)         -- Image/Data width
 is
   wi    : Natural := 0;
   hi    : Natural := 0;
   Min : Interfaces.IEEE_Float_32;
   Max : Interfaces.IEEE_Float_32;
   sd  : Interfaces.IEEE_Float_32;
 begin

  Find_MinMax_Float32(Data, Min, Max);
  Ada.Text_IO.Put_Line("Min " & Interfaces.IEEE_Float_32'Image(Min));
  Ada.Text_IO.Put_Line("Max " & Interfaces.IEEE_Float_32'Image(Max));

  for dd of Data
   loop

     -- sd := ( pixval'Last / Max ) * dd;
     sd := dd;

     if sd >= IEEE_Float_32(pixval'First) and
        sd <= IEEE_Float_32(pixval'Last)
     then

       Img(hi,wi) := Natural(sd);

     elsif sd < IEEE_Float_32(pixval'First)
     then

       Img(hi,wi) := pixval'First;

     else

       Img(hi,wi) := pixval'Last;

     end if;

     if wi = W-1 then
      hi := hi + 1;
      wi := 0;
     else
      wi := wi + 1;
     end if;
   end loop;

 end Convert_FITS_Float32_To_PNG_Int8;


 -- convert FITS to PNG image
 -- how to handle more then 2D files ?
 procedure FITS_To_PNG (FitsFileName : in String;
                        PngFileName  : in String;
                        HDUNum       : in Positive := 1)
 is
  FitsFile : SIO.File_Type;
  HDUSize  : HDU_Size_Type;
 begin

  --
  -- read FITS file:
  --
  SIO.Open(FitsFile,SIO.In_File,FitsFileName);

  Parse_HeaderBlocks(FitsFile,HDUSize);
   -- move behind the Header

  declare
     DataType : constant FitsData_Type := To_FitsDataType(HDUSize.DUSizeKeyVals.BITPIX);
     W : constant Dimension := Integer(HDUSize.DUSizeKeyVals.NAXISn(1));
     H : constant Dimension := Integer(HDUSize.DUSizeKeyVals.NAXISn(2));
                                  -- FIXME explicit cast!
     Data : DataArray_Type( DataType, W*H );
        -- holds data from FITS-file
     Img  : My_Image_Handle(0..(W-1), 0..(H-1));
        -- holds data for PNG image write
  begin
     Ada.Text_IO.Put_Line("DU type: " & FitsData_Type'Image(DataType));
     Ada.Text_IO.Put     (Integer'Image(W) & " x " );
     Ada.Text_IO.Put_Line(Integer'Image(H) );

     DataArray_Type'Read (SIO.Stream(FitsFile), Data);

     if DataType = Float32 then

        -- [FITS App. E] defines BigEndian byte order for IEEE Float32
        -- reverse byte order if system is LittleEndian
        if System.Default_Bit_Order = System.LOW_ORDER_FIRST
        then
          Endianness_Float32(Data.Float32Arr);
        end if;

        Convert_FITS_Float32_To_PNG_Int8(Data.Float32Arr, Img, W);

     elsif DataType = Int8 then

        Convert_FITS_Int8_To_PNG_Int8(Data.Int8Arr, Img, W);

     else

       Ada.Text_IO.Put_Line("Not implemented for "
                           & FitsData_Type'Image(DataType));

     end if;

   --
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
    Write_0(PngFileName, Img, H, W, Eight); --, D, I, L); Last 3 params have defaults
    --Write_0(PngFileName, Img, H, W, Sixteen); --, D, I, L); Last 3 params have defaults
   end;
   --
   -- END write PNG file
   --

   SIO.Close(FitsFile);

  end; -- 1st declare

 end FITS_To_PNG;

end Commands.PNG;

