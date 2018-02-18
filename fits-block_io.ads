
-- provides 'Read 'Write for Header- and Data Unit

-- covers two issues:
-- 1. Data alignment
-- 2. Endianness
--
-- Data structs reflect data-images inside FITS-files and so allow
-- (low level) Read / Write

with Interfaces;
with Ada.Streams.Stream_IO;


package FITS.Block_IO is

   type Unsigned_8 is new Interfaces.Unsigned_8;
   type Integer_16 is new Interfaces.Integer_16;
   type Integer_32 is new Interfaces.Integer_32;
   type Integer_64 is new Interfaces.Integer_64;
   type Float_32   is new Interfaces.IEEE_Float_32;
   type Float_64   is new Interfaces.IEEE_Float_64;

   procedure Float32_Read_BigEndian
    		(S    : access Ada.Streams.Root_Stream_Type'Class;
             	 Data : out Float_32 );

   procedure Float32_Write_BigEndian
    		(S    : access Ada.Streams.Root_Stream_Type'Class;
             	 Data : in Float_32 );

   for Float_32'Read  use Float32_Read_BigEndian;
   for Float_32'Write use Float32_Write_BigEndian;


   type Byte is mod 256;
   for Byte'Size use 8;
   -- [FITS] defines Byte as 8-bit

   BlockSize      : constant := 2880; -- in bytes
   BlockSize_bits : constant := BlockSize * Byte'Size;

   -- arrays

   type Float32Block_Arr is
     array ( 1 .. BlockSize_bits / Float_32'Size ) of Float_32;
   for Float32Block_Arr'Size use BlockSize_bits;
   pragma Pack (Float32Block_Arr);

private

   generic
     type Data_Type is private;
   procedure Revert_Bytes( Data : in out Data_Type );


end FITS.Block_IO;

