
-- FITS file's Data Unit

package FITS.Data is


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
