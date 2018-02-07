
with System;
use System;

with Ada.Unchecked_Conversion;

   -- [FITS 7.3 Binary Table extensions] p23: Big_Endian byte order & MSB bit first within a byte
   -- [FITS App. E] IEEE Floating Point format -> BigEndian byteorder
   --               bit order in 32bits: Fig E.1


package FitsFloat is


   -- Float32 FITS/IEEE format
   subtype Sign     is Natural range 0 .. 1;
   subtype Exponent is Natural range 0 .. 255;
   subtype Mantissa is Natural range 0 .. 2**(23-1);

   type FFloat32 is record
      s : Sign;
      e : Exponent;
      f : Mantissa;
   end record;
     pragma Pack(FFloat32);

   for FFloat32 use record
      s at 0 range 0 ..  0;
      e at 0 range 1 ..  8;
      f at 0 range 9 .. 31;
   end record;

   type FFloat32_LE is new FFloat32;
    for FFloat32_LE'Bit_Order use System.Low_Order_First;
    for FFloat32_LE'Scalar_Storage_Order use System.Low_Order_First;

   type FFloat32_BE is new FFloat32;
    for FFloat32_BE'Bit_Order use System.High_Order_First;
    for FFloat32_BE'Scalar_Storage_Order use System.High_Order_First;

  function FFloat32BE_To_Float is
    new Ada.Unchecked_Conversion(Source => FFloat32_BE, Target => Float);

  function FFloat32LE_To_Float is
    new Ada.Unchecked_Conversion(Source => FFloat32_LE, Target => Float);

end FitsFloat;
