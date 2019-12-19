
with Ada.Text_IO,
     Ada.Float_Text_IO,
     Ada.Unchecked_Deallocation,
     FITS,
     File,
     System,
     Ada.Streams.Stream_IO;

use
     FITS,
     File,
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



--package body Commands.PNG is
 --
 -- exported entry point
 --
 separate(Commands)
 procedure FITS_To_PNG (FitsFileName : in String;
                        PngFileName  : in String;
                        HDUNum       : in Positive := 1;
                        PlaneNum     : in Positive := 1)
 is

 -- grey (8bit) image

 type GreyPixel_8bit_Type is new Interfaces.Unsigned_8;

 type GreyImage_8bit_Type is
   array (Natural range <>, Natural range <>) of GreyPixel_8bit_Type;
 type GreyImage_8bit_Ptr is access GreyImage_8bit_Type;

 procedure Free_GreyImage_8bit is
   new Ada.Unchecked_Deallocation
      (Object => GreyImage_8bit_Type,
       Name   => GreyImage_8bit_Ptr);

 GreyPixel_8bit_Type_Last : constant Float_32 := 255.0;

 -- grey (16bit) image

 type GreyPixel_16bit_Type is new Interfaces.Unsigned_16;

 type GreyImage_16bit_Type is
   array (Natural range <>, Natural range <>) of GreyPixel_16bit_Type;
 type GreyImage_16bit_Ptr is access GreyImage_16bit_Type;

 procedure Free_GreyImage_16bit is
   new Ada.Unchecked_Deallocation
      (Object => GreyImage_16bit_Type,
       Name   => GreyImage_16bit_Ptr);

 GreyPixel_16bit_Type_Last : constant Float_32 := 65535.0;

 -- RGB (24bit 'Truecolor') image

 type RGBPixel_24bit_Type is new Interfaces.Unsigned_32;

 type RGBImage_24bit_Type is
   array (Natural range <>, Natural range <>) of RGBPixel_24bit_Type;
 type RGBImage_24bit_Ptr is access RGBImage_24bit_Type;

 procedure Free_RGBImage_24bit is
   new Ada.Unchecked_Deallocation
      (Object => RGBImage_24bit_Type,
       Name   => RGBImage_24bit_Ptr);

 RGBPixel_24bit_Type_Last : constant Float_32 := 16777215.0;


 -- instantiate Write-funcs for above types (from the package PNG_IO)

 -- write 8bit greyscale image

 function My_Grey8_Sample(I    : GreyImage_8bit_Ptr;
                          R, C : Coordinate) return GreyPixel_8bit_Type
 is
 begin
   return I(R,C);
 end My_Grey8_Sample;

 procedure Write_GreyImage_8bit is
   new Write_PNG_Type_0
       (GreyImage_8bit_Ptr,  -- image pixels
        GreyPixel_8bit_Type, -- pixel type
        My_Grey8_Sample);     -- func returns pixel for given [row, column]

 -- write 16bit greyscale image

 function My_Grey16_Sample(I    : GreyImage_16bit_Ptr;
                           R, C : Coordinate) return GreyPixel_16bit_Type
 is
 begin
   return I(R,C);
 end My_Grey16_Sample;

 procedure Write_GreyImage_16bit is
   new Write_PNG_Type_0
       (GreyImage_16bit_Ptr,  -- image pixels
        GreyPixel_16bit_Type, -- pixel type
        My_Grey16_Sample);     -- func returns pixel for given [row, column]

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

 -- Note: see [FITS] 8bit BITPIX type is UNSIGNED.
 -- All other FITS-integer types are signed. If those need unsigned range,
 -- an offset (BZERO key?) needs to be applied on them.
 procedure Convert_FITS_UInt8_To_PNG_8bit
--           (Data : in     UInt8Arr_Type;      -- FITS data
           (Data : in     UInt8_Arr;      -- FITS data
            Img  : in out GreyImage_8bit_Ptr; -- PNG pixels (the image)
            W    : in     Natural)            -- Image/Data width
 is
  wi    : Natural := 0;
  hi    : Natural := 0;
  dd    : FITS.Unsigned_8;
 begin

--  for dd of Data  <-- This is Ada2012 feature
  for ix in Data'Range
  loop

    dd := Data(ix);

    Img(wi,hi) := GreyPixel_8bit_Type(dd);
                 -- FIXME explicit conversion
    if wi = W-1 then
     hi := hi + 1;
     wi := 0;
    else
     wi := wi + 1;
    end if;
  end loop;

 end Convert_FITS_UInt8_To_PNG_8bit;



 procedure Convert_FITS_Float32_To_PNG_8bit
--           (Data : in     Float32Arr_Type;    -- FITS data
           (Data : in     Float32_Arr;    -- FITS data
            Min, Max :    Float_32; -- Min Max vals in FITS data
            Img  : in out GreyImage_8bit_Ptr; -- PNG pixels (the image)
            W    : in     Natural)            -- Image/Data width
 is
   wi    : Natural := 0;
   hi    : Natural := 0;
   Factor : Float_32;
   Val   : FITS.Float_32;
 begin

  Factor := GreyPixel_8bit_Type_Last / (Max - Min);

--  for Val of Data <--iterator is Ada2012 only
  for ix in Data'Range
   loop

     Val := Data(ix);

     Img.all(hi,wi) := GreyPixel_8bit_Type(Factor * (Val - Min));

     if wi = W-1 then
      hi := hi + 1;
      wi := 0;
     else
      wi := wi + 1;
     end if;
   end loop;

 end Convert_FITS_Float32_To_PNG_8bit;


 procedure Convert_FITS_Float32_To_PNG_16bit
--           (Data : in     Float32Arr_Type;     -- FITS data
           (Data : in     Float32_Arr;     -- FITS data
            Min, Max : Float_32; -- Min Max vals in FITS data
            Img  : in out GreyImage_16bit_Ptr; -- PNG pixels (the image)
            W    : in     Natural)             -- Image/Data width
 is
   wi    : Natural := 0;
   hi    : Natural := 0;
   Factor : Float_32;
   Val   : FITS.Float_32;
 begin

  Factor := GreyPixel_16bit_Type_Last / (Max - Min);

--  for Val of Data <--iterator is Ada2012 only
  for ix in Data'Range
   loop

     Val := Data(ix);

     Img.all(hi,wi) := GreyPixel_16bit_Type(Factor * (Val - Min));

     if wi = W-1 then
      hi := hi + 1;
      wi := 0;
     else
      wi := wi + 1;
     end if;
   end loop;

 end Convert_FITS_Float32_To_PNG_16bit;


   -- find minimum and maximum value of the Float32 data array
   procedure Find_MinMax_Float32
--              (F32Arr : in  Float32Arr_Type;
              (F32Arr : in  Float32_Arr;
               Min    : out Float_32;
               Max    : out Float_32)
   is  
     type MyFloat is new Float_32;
     D   : FITS.Float_32;
   begin

     Min := Float_32'Large;
     Max := Float_32'Small;

     --for D of F32Arr  Ada2012 feature
     for ix in F32Arr'Range
      loop

       D := F32Arr(ix);

       if D > Max then Max := D; end if;
       if D < Min then Min := D; end if;

     end loop;

   end Find_MinMax_Float32;




 procedure Convert_FITS_Float32_To_PNG_24rgb
--           (Data : in     Float32Arr_Type;    -- FITS data
           (Data : in     Float32_Arr;    -- FITS data
            Min, Max : Float_32; -- Min Max vals in FITS data
-- FIXME img should be IN or OUT or IN OUT ? It is a pointer type
            Img  : in out RGBImage_24bit_Ptr; -- PNG pixels (the image)
            W    : in     Natural)            -- Image/Data width
 is
   wi    : Natural := 0;
   hi    : Natural := 0;
   Factor : Float_32;
   Val   : FITS.Float_32;
   flast  : Float_32 :=
            Float_32(RGBPixel_24bit_Type'Last);
 begin

-- debug: suspect rounding incorrect for 'flast'
--  Ada.Float_Text_IO.Put(Float(flast),2,15,3);
--  Ada.Float_Text_IO.Put(Float(RGBPixel_24bit_Type_Last),2,15,3);
--  Ada.Text_IO.New_Line;

  factor :=  (RGBPixel_24bit_Type_Last-1.0) / (Max - Min);
  -- FIXME needs -1.0 otherwise overflow error
  --       on Float -> Unsigned24 conversion

--  for Val of Data
--   loop
  for ix in Data'Range
   loop

     Val := Data(ix);

     Img.all(hi,wi) := RGBPixel_24bit_Type( Factor * (Val - Min) );
             --  and RGBPixel_24bit_Type(16#00FF_FFFF#); -- <--FIXME do we need this ? Pixel is defined 24bit!

     if wi = W-1 then
      hi := hi + 1;
      wi := 0;
     else
      wi := wi + 1;
     end if;
   end loop;

 end Convert_FITS_Float32_To_PNG_24rgb;


  -- Main Entry

  FitsFile : SIO.File_Type;
 begin

  -- read FITS file:
  SIO.Open(FitsFile,SIO.In_File,FitsFileName);

  declare
     HDUInfo  : HDU_Info_Type := Read_Header(FitsFile);
     DataType : constant Data_Type     := To_DataType(HDUInfo.BITPIX);
     W        : constant Dimension     := Integer(HDUInfo.NAXISn(1));
     H        : constant Dimension     := Integer(HDUInfo.NAXISn(2));
                                          -- FIXME explicit cast!
     Min    : Float_32;
     Max    : Float_32;

     -- below Data and Img's are on Heap (instead of Stack)
     -- due to their size: we read into memory all FITS DataUnit
     -- and we rely on virtual memory to have
     -- enough space in case of big files

--     type Data_Arr_Ptr is access Data_Arr( DataType, FPositive(W*H) );
--     Data : Data_Arr_Ptr  := new Data_Arr( DataType, FPositive(W*H) ) ;
--     type DataArray_Ptr is access DataArray_Type( DataType, W*H );
--     Data : DataArray_Ptr  := new DataArray_Type( DataType, W*H ) ;
     -- holds data from FITS-file
     -- will be de-alloc at exit from begin .. end section

     Img    : GreyImage_8bit_Ptr;
     -- holds data for PNG image write

     Img16  : GreyImage_16bit_Ptr;
     -- holds data for PNG image write

     RGBImg : RGBImage_24bit_Ptr;
     -- holds data for PNG RGBimage write

     IdxPlaneNum : SIO.Count := SIO.Index(FitsFile)
                              + SIO.Count((PlaneNum-1)*(W*H*4));
                              -- FIXME Explicit conversion
                              -- FIXME Float32 size (*4) given explicitely
  begin
     Ada.Text_IO.Put_Line("DU type: " & Data_Type'Image(DataType));
     Ada.Text_IO.Put     (Integer'Image(W) & " x " );
     Ada.Text_IO.Put_Line(Integer'Image(H) );

     -- skip planes up to PlaneNum
     Ada.Streams.Stream_IO.Set_Index(FitsFile,IdxPlaneNum);

     if DataType = Float32 then

        declare
         -- allocate Data on Heap
         type Float32_Arr_Ptr is access Float32_Arr( 1 .. FPositive(W*H) );
         Data : Float32_Arr_Ptr := new Float32_Arr( 1 .. FPositive(W*H) );
        begin

--        Data_Arr'Read (SIO.Stream(FitsFile), Data.all);
        Float32_Arr'Read (SIO.Stream(FitsFile), Data.all);

--        Find_MinMax_Float32(Data.all.Float32Arr, Min, Max);
        Find_MinMax_Float32(Data.all, Min, Max);
        Ada.Text_IO.Put_Line("Min " & Float_32'Image(Min));
        Ada.Text_IO.Put_Line("Max " & Float_32'Image(Max));

        -- Write  8bit Greyscale Image
        Img := new GreyImage_8bit_Type(0..(W-1), 0..(H-1));
--        Convert_FITS_Float32_To_PNG_8bit(Data.Float32Arr, Min, Max, Img, W);
        Convert_FITS_Float32_To_PNG_8bit(Data.all, Min, Max, Img, W);
        Write_GreyImage_8bit(PngFileName, Img, H, W); --, D, I, L); Last 3 params have defaults
        Free_GreyImage_8bit(Img);

        -- Write 16bit Greyscale Image
        Img16 := new GreyImage_16bit_Type(0..(W-1), 0..(H-1));
--        Convert_FITS_Float32_To_PNG_16bit(Data.Float32Arr, Min, Max, Img16, W);
        Convert_FITS_Float32_To_PNG_16bit(Data.all, Min, Max, Img16, W);
        Write_GreyImage_16bit(PngFileName&"_G16.png", Img16, H, W, Sixteen); --, D, I, L); Last 3 params have defaults
        Free_GreyImage_16bit(Img16);

        -- Write Truecolor Image
        RGBImg := new RGBImage_24bit_Type(0..(W-1), 0..(H-1));
--        Convert_FITS_Float32_To_PNG_24rgb(Data.Float32Arr, Min, Max, RGBImg, W);
        Convert_FITS_Float32_To_PNG_24rgb(Data.all, Min, Max, RGBImg, W);
        Write_RGBImage_24bit(PngFileName&".png", RGBImg, H, W);
                             -- Eight, False, Null_Chunk_List, Best_Compression);
                             -- No_Compression Best_Speed Best_Compression Default_Compression
        Free_RGBImage_24bit(RGBImg);

        end;

     elsif DataType = UInt8 then

        declare
         -- allocate Data on Heap
         type UInt8_Arr_Ptr is access UInt8_Arr( 1 .. FPositive(W*H) );
         Data   : UInt8_Arr_Ptr   := new UInt8_Arr( 1 .. FPositive(W*H) );
        begin

--         Data_Arr'Read (SIO.Stream(FitsFile), Data.all);
         UInt8_Arr'Read (SIO.Stream(FitsFile), Data.all);

         Img := new GreyImage_8bit_Type(0..(W-1), 0..(H-1));
--         Convert_FITS_UInt8_To_PNG_8bit(Data.UInt8Arr, Img, W);
         Convert_FITS_UInt8_To_PNG_8bit(Data.all, Img, W);
         Write_GreyImage_8bit(PngFileName, Img, H, W); --, D, I, L); Last 3 params have defaults
         Free_GreyImage_8bit(Img);

        end;

     else

       Ada.Text_IO.Put_Line("Not implemented for "
                           & Data_Type'Image(DataType));

     end if;


   SIO.Close(FitsFile);

  end; -- release all Heap memory alloc made in declare .. begin ...

 end FITS_To_PNG;

--end Commands.PNG;

