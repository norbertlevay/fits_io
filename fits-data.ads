
-- FITS file's Data Unit

with Interfaces;
with Ada.Streams.Stream_IO;

with FITS.Size; use FITS.Size;

package FITS.Data is

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

   procedure Float32_Read_BigEndian
    		(S    : access Ada.Streams.Root_Stream_Type'Class;
             	 Data : out Float_32 );

   procedure Float32_Write_BigEndian
    		(S    : access Ada.Streams.Root_Stream_Type'Class;
             	 Data : in Float_32 );

   for Float_32'Read  use Float32_Read_BigEndian;
   for Float_32'Write use Float32_Write_BigEndian;


   -- Data Unit arrays

   type UInt8Arr_Type   is array ( FPositive range <> ) of Unsigned_8;
   type Int16Arr_Type   is array ( FPositive range <> ) of Integer_16;
   type Int32Arr_Type   is array ( FPositive range <> ) of Integer_32;
   type Int64Arr_Type   is array ( FPositive range <> ) of Integer_64;
   type Float32Arr_Type is array ( FPositive range <> ) of Float_32;
   type Float64Arr_Type is array ( FPositive range <> ) of Float_64;

   procedure Find_MinMax_Float32
              (F32Arr : in  Float32Arr_Type;
               Min    : out Float_32;
               Max    : out Float_32);
   -- find minimum and maximum value of the Float32 data array

   type Data_Type is
       (UInt8,   Int16,
        Int32,   Int64,
        Float32, Float64);
   -- [FITS, Sect 4.4.1.1 Table 8]

   function  To_DataType (BITPIX : in Integer) return Data_Type;

   type Data_Arr ( FitsType : Data_Type ;
                   Length   : FPositive ) is
     record
       case FitsType is
       when UInt8 =>   UInt8Arr   : UInt8Arr_Type(1 .. Length);
       when Int16 =>   Int16Arr   : Int16Arr_Type(1 .. Length);
       when Int32 =>   Int32Arr   : Int32Arr_Type(1 .. Length);
       when Int64 =>   Int64Arr   : Int64Arr_Type(1 .. Length);
       when Float32 => Float32Arr : Float32Arr_Type(1 .. Length);
       when Float64 => Float64Arr : Float64Arr_Type(1 .. Length);
      end case;
     end record;

   -- in file all data are packed

   pragma Pack (UInt8Arr_Type);
   pragma Pack (Int16Arr_Type);
   pragma Pack (Int32Arr_Type);
   pragma Pack (Int64Arr_Type);
   pragma Pack (Float32Arr_Type);
   pragma Pack (Float64Arr_Type);

end FITS.Data;
