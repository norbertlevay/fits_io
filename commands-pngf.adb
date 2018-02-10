
with Ada.Text_IO,
     Ada.Float_Text_IO,
     FITS,
     FITS.File,
     System,
     Ada.Unchecked_Conversion,
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


package body Commands.PNGf is

 -- grey 8bit image
 Depth : PNG_IO.Depth := Eight;
 subtype TPixVal16 is Natural range 0 .. 255; -- 2**8 - 1
 TPixVal16_Last : constant Interfaces.IEEE_Float_32 := 255.0;

 -- Depth : PNG_IO.Depth := Sixteen;
 -- grey 16bit image
-- subtype TPixVal16 is Natural range 0 .. 65535; -- 2**16 - 1
-- TPixVal16_Last : constant Interfaces.IEEE_Float_32 := 65535.0;

 type My_Image_Handle is
  record
    FitsFile : SIO.File_Type;
    DUStart  : SIO.Count;
    Min,Max  : Interfaces.IEEE_Float_32;
  end record;

 procedure DU_MinMax(File     : in SIO.File_Type;
                     DUSize   : in Count;
                     Min, Max : out Interfaces.IEEE_Float_32)
 is
   chunkSize : constant Count := 2880;
   ReadSize  : Count := 0;
   F32Arr : Float32Arr_Type(1 .. 2880/4);
   locMin : Interfaces.IEEE_Float_32;
   locMax : Interfaces.IEEE_Float_32;
 begin

   Min := Interfaces.IEEE_Float_32'Large;
   Max := Interfaces.IEEE_Float_32'Small;


   while ReadSize < DUSize
   loop

     Float32Arr_Type'Read(SIO.Stream(File), F32Arr);
     ReadSize := ReadSize + chunkSize;

     Endianness_Float32( F32Arr );

     locMin := Interfaces.IEEE_Float_32'Large;
     locMax := Interfaces.IEEE_Float_32'Small;
     Find_MinMax_Float32(F32Arr,locMin,locMax);

     if locMin < Min then Min := locMin; end if;
     if locMax > Max then Max := locMax; end if;

   end loop;

 end DU_MinMax;


 function Scale_FITS_Float32_To_PNG_UInt16
          (Min : in Interfaces.IEEE_Float_32;
           Max : in Interfaces.IEEE_Float_32;
           Val : in Interfaces.IEEE_Float_32) return TPixVal16
 is
   Factor : Interfaces.IEEE_Float_32 := (Val - Min) / (Max - Min);
 begin
   return TPixVal16(TPixVal16_Last * Factor);
 end Scale_FITS_Float32_To_PNG_UInt16;

   -- reverse byte order for each FLoat32:
   --  4->1 3->2 2->3 1->4
   procedure Reverse_Float32( F32 : in out Interfaces.IEEE_Float_32 )
   is
     type MyFloat is new Interfaces.IEEE_Float_32;
     type Arr4xU8 is array (1..4) of Interfaces.Unsigned_8;

     function MyFloat_To_Arr is
       new Ada.Unchecked_Conversion(Source => MyFloat, Target => Arr4xU8);
     function Arr_To_MyFloat is
       new Ada.Unchecked_Conversion(Source => Arr4xU8, Target => MyFloat);

     procedure SwapBytes(arr : in out Arr4xU8) is
      temp : Arr4xU8;
     begin
      temp(1) := arr(4);
      temp(2) := arr(3);
      temp(3) := arr(2);
      temp(4) := arr(1);
      arr := temp;
     end SwapBytes;

     aaaa : Arr4xU8;
   begin
       aaaa := MyFloat_To_Arr(MyFloat(F32));
       SwapBytes(aaaa);
       F32 := Interfaces.IEEE_Float_32(Arr_To_MyFloat(aaaa));
   end Reverse_Float32;



  function My_Grey_Sample(FITS_Data : My_Image_Handle;
                          R, C : Coordinate) return TPixVal16
  is
    Data : Interfaces.IEEE_Float_32;
    D16  : TPixVal16;
  begin

    -- here Offset = CalcOffset_Float32Arr(R,C);
    -- SIO.Set_Index(FITS_Data.FitsFile, FITS_Data.DUStart + Offset )

    Ada.Text_IO.Put(Count'Image(Index(FITS_Data.FitsFile)));
    Interfaces.IEEE_Float_32'Read (SIO.Stream(FITS_Data.FitsFile), Data);

    -- [FITS App. E] defines BigEndian byte order for IEEE Float32
    -- reverse byte order if system is LittleEndian
    if System.Default_Bit_Order = System.LOW_ORDER_FIRST
    then
      Reverse_Float32(Data);
    end if;

     Ada.Text_IO.Put(" [" & Coordinate'Image(R) & "," & Coordinate'Image(C)&"] ");
     Ada.Float_Text_IO.Put(Float(Data), 2, 15, 3);

    D16 := Scale_FITS_Float32_To_PNG_UInt16(FITS_Data.Min,FITS_Data.Max, Data);

     Ada.Text_IO.Put(" -> " & TPixVal16'Image(D16));
     Ada.Text_IO.New_Line;

    return D16;
  end My_Grey_Sample;




 -- convert FITS to PNG image
 -- how to handle more then 2D files ?
 procedure FITS_To_PNG (FitsFileName : in String;
                        PngFileName  : in String;
                        HDUNum       : in Positive := 1;
                        PlaneNum     : in Positive := 1)
 is
  FITS_Data : My_Image_Handle;
  HDUSize   : HDU_Size_Type;
  DUSize    : Count;
  procedure Write_0 is
    new Write_PNG_Type_0(My_Image_Handle, TPixVal16, My_Grey_Sample);
 begin

  --
  -- read FITS file:
  --
  SIO.Open(FITS_Data.FitsFile,SIO.In_File,FitsFileName);

  Parse_HeaderBlocks(FITS_Data.FitsFile,HDUSize);
   -- move behind the Header

  FITS_Data.DUStart := SIO.Index(FITS_Data.FitsFile);

  DUSize := 2880 * Count(Size_blocks(HDUSize.DUSizeKeyVals));

  DU_MinMax(FITS_Data.FitsFile, DUSize,
            FITS_Data.Min,  FITS_Data.Max );

  SIO.Set_Index(FITS_Data.FitsFile,FITS_Data.DUStart);
  -- set back to DU Start

  declare
     DataType : constant FitsData_Type := To_FitsDataType(HDUSize.DUSizeKeyVals.BITPIX);
     W : constant Dimension := Integer(HDUSize.DUSizeKeyVals.NAXISn(1));
     H : constant Dimension := Integer(HDUSize.DUSizeKeyVals.NAXISn(2));
                                  -- FIXME explicit cast!
     IdxPlaneNum : SIO.Count := SIO.Index(FITS_Data.FitsFile)
                              + SIO.Count((PlaneNum-1)*(W*H*4));
                              -- FIXME Explicit conversion
                              -- FIXME Float32 size (*4) given explicitely
  begin
     Ada.Text_IO.Put_Line("DU type: " & FitsData_Type'Image(DataType));
     Ada.Text_IO.Put     (Integer'Image(W) & " x " );
     Ada.Text_IO.Put_Line(Integer'Image(H) );
     Ada.Text_IO.Put_Line("Min " & Interfaces.IEEE_Float_32'Image(FITS_Data.Min));
     Ada.Text_IO.Put_Line("Max " & Interfaces.IEEE_Float_32'Image(FITS_Data.Max));

     -- skip planes before PlaneNum
     Ada.Streams.Stream_IO.Set_Index(FITS_Data.FitsFile,IdxPlaneNum);

     if DataType = Float32 then

        Write_0(PngFileName,FITS_Data,W,H,Depth);
        -- internally calls:
        -- My_Grea_Sample(FitsFile, R,C) return UInt16bit

     else

       Ada.Text_IO.Put_Line("FITS_To_PNGf Not implemented for "
                           & FitsData_Type'Image(DataType));

     end if;

  end;-- declare

  SIO.Close(FITS_Data.FitsFile);

 end FITS_To_PNG;

end Commands.PNGf;

