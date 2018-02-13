
with Ada.Text_IO,
     Ada.Float_Text_IO,
     FITS,
     FITS.File,
     System,
     Ada.Streams.Stream_IO;

use
     FITS,
     FITS.File,
     System,
     Ada.Streams.Stream_IO;

with PNG_IO;
use  PNG_IO;

with Interfaces;
use  Interfaces;

-- Note: print Float with 2 digits in front, 15 digits after decimal, 3 digits exponent
--     Ada.Float_Text_IO.Put(Float(MaxPixelF32), 2, 15, 3);
--     Ada.Text_IO.New_Line;
-- Note: Conversion of Unsigned_32'Last (2**32-1) to Float yields value bigger by one (2**32) !!??

-- Different modes to define N-bit Integer:
-- subtype RGBpixval is Natural range 0 .. ((2**24) -1);
-- type RGBpixval is mod 2**24; -- 24bit unsigned integer
-- type RGBpixval is new Interfaces.Unsigned_24;
 --
 -- Verify BUG(?):
 -- FIXME Convert to float incorrect if Float(Int24'Last) gives: 16777216.0  VERIFY!!!!
 -- In any case Last valid value for scaling is: Float(2**24 - 1) = 16777215.0
 -- FIXME which type def for 24bit RGB: 32bit or 24bit for storage ? (Max value is always 24bit)

-- Note on zlib exception: (From Convert Float32 to rgb24)
-- value has effect probably because compression depends on values, and so compression affects buffer indexes
-- /5100; dividing pixel-value by 5100 eliminates exception in zlib.adb:542

-- FIXME how to handle more then 2D files ?

   --
   -- write PNG file:
   --
   -- requires instantiation of generic Write_PNG_Type_0() func from PNG_IO.ads
   -- needs 3 things:
   --   something what holds the pixels -> Image_Handle - below not used
   --   type of one pixel/Sample -> below pixval (derived from Natural)
   --   written function which returns each pixel by coordinates from the Image_Handle
   -- Read the long comment in: png_io.ads l.367 before generic decl of Write_PNG_Type_0



package body Commands.PNG is

 -- grey (8bit) image

 type GreyPixel_8bit_Type is new Interfaces.Unsigned_8;
 type GreyImage_8bit_Type is
   array (Natural range <>, Natural range <>) of GreyPixel_8bit_Type;
 type GreyImage_8bit_Ptr is access GreyImage_8bit_Type;

 GreyPixel_8bit_Type_Last : constant Interfaces.IEEE_Float_32 := 255.0;

 -- RGB (24bit 'Truecolor') image

 type RGBPixel_24bit_Type is new Interfaces.Unsigned_24;
 type RGBImage_24bit_Type is
   array (Natural range <>, Natural range <>) of RGBPixel_24bit_Type;
 type RGBImage_24bit_Ptr is access RGBImage_24bit_Type;

 RGBPixel_24bit_Type_Last : constant Interfaces.IEEE_Float_32 := 16777215.0;


 -- instantiate Write-funcs for above types (from the package PNG_IO)

 -- write greyscale image

 function My_Grey_Sample(I    : GreyImage_8bit_Ptr;
                         R, C : Coordinate) return GreyPixel_8bit_Type
 is
 begin
   return I(R,C);
 end My_Grey_Sample;

 procedure Write_GreyImage_8bit is
   new Write_PNG_Type_0
       (GreyImage_8bit_Ptr,  -- image pixels
        GreyPixel_8bit_Type, -- pixel type
        My_Grey_Sample);     -- func returns pixel for given [row, column]

 -- write RGB 'Truecolor' image

 function My_Red_Value(RGBImg  : RGBImage_24bit_Ptr;
                       R, C    : Coordinate) return RGBPixel_24bit_Type
 is
   U32 : Unsigned_32 := Unsigned_32(RGBImg(R,C));
 begin
   return RGBPixel_24bit_Type(Shift_Right(U32, 16) and 16#0000_00FF#);
 end My_Red_Value;

 function My_Green_Value(RGBImg : RGBImage_24bit_Ptr;
                         R, C   : Coordinate) return RGBPixel_24bit_Type
 is
   U32 : Unsigned_32 := Unsigned_32(RGBImg(R,C));
 begin
   return RGBPixel_24bit_Type(Shift_Right(U32,  8) and 16#0000_00FF#);
 end My_Green_Value;

 function My_Blue_Value(RGBImg : RGBImage_24bit_Ptr;
                        R, C   : Coordinate) return RGBPixel_24bit_Type
 is
   U32 : Unsigned_32 := Unsigned_32(RGBImg(R,C));
 begin
   return RGBPixel_24bit_Type(U32 and 16#0000_00FF#);
 end My_Blue_Value;

 procedure Write_RGBImage_24bit is
   new Write_PNG_Type_2
       (RGBImage_24bit_Ptr,  -- image pixels
        RGBPixel_24bit_Type, -- pixel type
        My_Red_Value,        -- funcs: return Red pixel for given [row, column]
        My_Green_Value,      --               Green
        My_Blue_Value);      --               Blue



--
-- conversions from FITS data to PNG pixels
--

 -- FIXME see [FITS] 8bit BITPIX type is UNSIGNED.
 -- All other FITS-integer types are signed. If those need unsigned range,
 -- an offset (BZERO key?) needs to be applied on them.
 procedure Convert_FITS_Int8_To_PNG_Int8
           (Data : in     Int8Arr_Type;       -- FITS data
            Img  : in out GreyImage_8bit_Ptr; -- PNG pixels (the image)
            W    : in     Natural)            -- Image/Data width
 is
  wi    : Natural := 0;
  hi    : Natural := 0;
 begin

  for dd of Data
  loop

    Img(wi,hi) := GreyPixel_8bit_Type(dd) + 128;
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
           (Data : in     Float32Arr_Type;    -- FITS data
            Img  : in out GreyImage_8bit_Ptr; -- PNG pixels (the image)
            W    : in     Natural)            -- Image/Data width
 is
   wi    : Natural := 0;
   hi    : Natural := 0;
   Min    : Interfaces.IEEE_Float_32;
   Max    : Interfaces.IEEE_Float_32;
   Factor : Interfaces.IEEE_Float_32;
 begin

  Find_MinMax_Float32(Data, Min, Max);
  Ada.Text_IO.Put_Line("Min " & Interfaces.IEEE_Float_32'Image(Min));
  Ada.Text_IO.Put_Line("Max " & Interfaces.IEEE_Float_32'Image(Max));

  Factor := GreyPixel_8bit_Type_Last / (Max - Min);

  for Val of Data
   loop

     Img.all(hi,wi) := GreyPixel_8bit_Type(Factor * (Val - Min));

     if wi = W-1 then
      hi := hi + 1;
      wi := 0;
     else
      wi := wi + 1;
     end if;
   end loop;

 end Convert_FITS_Float32_To_PNG_Int8;


 procedure Convert_FITS_Float32_To_PNG_rgb24
           (Data : in     Float32Arr_Type;    -- FITS data
-- FIXME img should be IN or OUT or IN OUT ? It is a pointer type
            Img  : in out RGBImage_24bit_Ptr; -- PNG pixels (the image)
            W    : in     Natural)            -- Image/Data width
 is
   wi    : Natural := 0;
   hi    : Natural := 0;
   Min    : Interfaces.IEEE_Float_32;
   Max    : Interfaces.IEEE_Float_32;
   Factor : Interfaces.IEEE_Float_32;
   flast  : Interfaces.IEEE_Float_32 :=
            Interfaces.IEEE_Float_32(RGBPixel_24bit_Type'Last);
 begin

  Find_MinMax_Float32(Data, Min, Max);
  Ada.Text_IO.Put_Line("Min " & Interfaces.IEEE_Float_32'Image(Min));
  Ada.Text_IO.Put_Line("Max " & Interfaces.IEEE_Float_32'Image(Max));

  Ada.Float_Text_IO.Put(Float(flast),2,15,3);
  Ada.Float_Text_IO.Put(Float(RGBPixel_24bit_Type_Last),2,15,3);
  Ada.Text_IO.New_Line;

  factor :=  (RGBPixel_24bit_Type_Last-1.0) / (Max - Min);
  -- FIXME needs -1.0 otherwise overflow error
  --       on Float -> Unsigned24 conversion

  for Val of Data
   loop

     Img.all(hi,wi) := RGBPixel_24bit_Type( Factor * (Val - Min) );
             --  and RGBPixel_24bit_Type(16#00FF_FFFF#); -- <--FIXME do we need this ? Pixel is defined 24bit!

     if wi = W-1 then
      hi := hi + 1;
      wi := 0;
     else
      wi := wi + 1;
     end if;
   end loop;

 end Convert_FITS_Float32_To_PNG_rgb24;



 --
 -- exported entry point
 --
 procedure FITS_To_PNG (FitsFileName : in String;
                        PngFileName  : in String;
                        HDUNum       : in Positive := 1;
                        PlaneNum     : in Positive := 1)
 is
  FitsFile : SIO.File_Type;
  HDUSize  : HDU_Size_Type;
 begin

  -- read FITS file:
  SIO.Open(FitsFile,SIO.In_File,FitsFileName);

  Parse_HeaderBlocks(FitsFile,HDUSize);
   -- move behind the Header

  declare
     DataType : constant FitsData_Type := To_FitsDataType(HDUSize.DUSizeKeyVals.BITPIX);
     W        : constant Dimension     := Integer(HDUSize.DUSizeKeyVals.NAXISn(1));
     H        : constant Dimension     := Integer(HDUSize.DUSizeKeyVals.NAXISn(2));
                                          -- FIXME explicit cast!

     -- below all 3 new heap memory allocations
     -- will be freed automatically at exit from begin..end section

     type DataArray_Ptr is access DataArray_Type( DataType, W*H );
     Data : DataArray_Ptr  := new DataArray_Type( DataType, W*H ) ;
     -- holds data from FITS-file

     Img    : GreyImage_8bit_Ptr := new GreyImage_8bit_Type(0..(W-1), 0..(H-1));
     -- holds data for PNG image write

     RGBImg : RGBImage_24bit_Ptr := new RGBImage_24bit_Type(0..(W-1), 0..(H-1));
     -- holds data for PNG RGBimage write

     IdxPlaneNum : SIO.Count := SIO.Index(FitsFile)
                              + SIO.Count((PlaneNum-1)*(W*H*4));
                              -- FIXME Explicit conversion
                              -- FIXME Float32 size (*4) given explicitely
  begin
     Ada.Text_IO.Put_Line("DU type: " & FitsData_Type'Image(DataType));
     Ada.Text_IO.Put     (Integer'Image(W) & " x " );
     Ada.Text_IO.Put_Line(Integer'Image(H) );

     -- skip planes before PlaneNum
     Ada.Streams.Stream_IO.Set_Index(FitsFile,IdxPlaneNum);

     DataArray_Type'Read (SIO.Stream(FitsFile), Data.all);

     if DataType = Float32 then

        -- [FITS App. E] defines BigEndian byte order for IEEE Float32
        -- reverse byte order if system is LittleEndian
        if System.Default_Bit_Order = System.LOW_ORDER_FIRST
        then
          Endianness_Float32(Data.Float32Arr);
        end if;

        -- Write Greyscale Image
        Convert_FITS_Float32_To_PNG_Int8(Data.Float32Arr, Img, W);
        Write_GreyImage_8bit(PngFileName, Img, H, W); --, D, I, L); Last 3 params have defaults

        -- Write Truecolor Image
        Convert_FITS_Float32_To_PNG_rgb24(Data.Float32Arr, RGBImg, W);
        Write_RGBImage_24bit(PngFileName&".png", RGBImg, H, W,
                             Eight, False, Null_Chunk_List, No_Compression);

     elsif DataType = Int8 then

        Convert_FITS_Int8_To_PNG_Int8(Data.Int8Arr, Img, W);
        Write_GreyImage_8bit(PngFileName, Img, H, W); --, D, I, L); Last 3 params have defaults

     else

       Ada.Text_IO.Put_Line("Not implemented for "
                           & FitsData_Type'Image(DataType));

     end if;


   SIO.Close(FitsFile);

  end; -- release all Heap memory alloc made after begin

 end FITS_To_PNG;

end Commands.PNG;

