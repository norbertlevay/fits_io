
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

   type Byte is mod 256;
   for Byte'Size use 8;
   -- [FITS] defines Byte as 8-bit

   BlockSize      : constant := 2880; -- in bytes
   BlockSize_bits : constant := BlockSize * Byte'Size;

   generic
     type Data_Type is private;
   package DataBlock is

    type DataBlock_Type is
      array ( 1 .. BlockSize_bits / Data_Type'Size ) of Data_Type;
    pragma Pack (DataBlock_Type);

    procedure Write_BigEndian
     		(S    : access Ada.Streams.Root_Stream_Type'Class;
              	 Data : in Data_Type );
   private
    procedure Revert_Bytes( Data : in out Data_Type );
   end DataBlock;


private

    procedure Write_BigEndian_Float32
     		(S    : access Ada.Streams.Root_Stream_Type'Class;
              	 Data : in Float_32 );
    for Float_32'Write use Write_BigEndian_Float32;

----------------------------------------------------
-- examples if DataBlock_Type would be instatioated
-- then representational clases can be given

--   procedure Write_BigEndian_Float32
--    		(S    : access Ada.Streams.Root_Stream_Type'Class;
--             	 Data : in Float_32 ) is null;
--   for Float_32'Write use Write_BigEndian_Float32;

--   type DataBlockF32_Type is
--     array (1..(2880*8)/Float_32'Size) of Float_32;
--   for DataBlockF32_Type'Size use (2880*Float_32'Size);

end FITS.Block_IO;

