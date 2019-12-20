--with System.Storage_Elements; use System.Storage_Elements;

with Interfaces;
with Ada.Streams.Stream_IO;

with Keyword_Record; use Keyword_Record; -- FInteger needed

package Image is

   type Data_Type is
       (UInt8,   Int16,
        Int32,   Int64,
        Float32, Float64);
   -- [FITS, Sect 4.4.1.1 Table 8]

   function  To_DataType (BITPIX : in Integer) return Data_Type;

   -- [FITS Sect 5.2 .. 5.3] says that 8bit is UNSIGNED
   -- all others are SIGNED (see  Table 8)
   -- If unsigned needed for Int16..Int64 BZERO keyword is used
   -- to shift the value range (see Table 11)

   type Unsigned_8 is new Interfaces.Unsigned_8;
   type Integer_16 is new Interfaces.Integer_16;
   type Integer_32 is new Interfaces.Integer_32;
   type Integer_64 is new Interfaces.Integer_64;
   type Float_32   is new Interfaces.IEEE_Float_32;
   type Float_64   is new Interfaces.IEEE_Float_64;

   -- [FITS] defines BigEndian for all numeric types in file
   -- revert byte order when reading/writing from/to FITS file

   type UInt8_Arr   is array ( FPositive range <> ) of Unsigned_8;
   type Int16_Arr   is array ( FPositive range <> ) of Integer_16;
   type Int32_Arr   is array ( FPositive range <> ) of Integer_32;
   type Int64_Arr   is array ( FPositive range <> ) of Integer_64;
   type Float32_Arr is array ( FPositive range <> ) of Float_32;
   type Float64_Arr is array ( FPositive range <> ) of Float_64;



private

   procedure Float32_Read_BigEndian
    		(S    : access Ada.Streams.Root_Stream_Type'Class;
             	 Data : out Float_32 );

   procedure Float32_Write_BigEndian
    		(S    : access Ada.Streams.Root_Stream_Type'Class;
             	 Data : in Float_32 );

   for Float_32'Read  use Float32_Read_BigEndian;
   for Float_32'Write use Float32_Write_BigEndian;

end Image;

