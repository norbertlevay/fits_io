
with Interfaces;	use Interfaces;
with Ada.Streams;

package Data_Types is

   type Unsigned_8 is new Interfaces.Unsigned_8;
   type Integer_16 is new Interfaces.Integer_16;
   type Integer_32 is new Interfaces.Integer_32;
   type Integer_64 is new Interfaces.Integer_64;
   type Float_32   is new Interfaces.IEEE_Float_32;
   type Float_64   is new Interfaces.IEEE_Float_64;



   procedure Int32_Read_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : out Integer_32 );

   procedure Int32_Write_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : in Integer_32 );

   for Integer_32'Read  use Int32_Read_BigEndian;
   for Integer_32'Write use Int32_Write_BigEndian;




   procedure Float32_Read_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : out Float_32 );

   procedure Float32_Write_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : in Float_32 );

   for Float_32'Read  use Float32_Read_BigEndian;
   for Float_32'Write use Float32_Write_BigEndian;


-- define generic Block

generic
        type T is private;
package Data is

Block_Size : constant Positive := 2880*8;

N : constant Positive := Block_Size / T'Size;
-- FIXME how-to: should refuse to instantiate for T if above division is not without reminder
-- FIXME how-to: guarantee that array is packed for any T

type Block is array (Positive range 1 .. N) of T;

end Data;

-- use Block

package Data_UInt8 is new Data(T => Unsigned_8);
package Data_Int16 is new Data(T => Integer_16);
package Data_Int32 is new Data(T => Integer_32);
package Data_Int64 is new Data(T => Integer_64);

package Data_Float32 is new Data(T => Float_32);
package Data_Float64 is new Data(T => Float_64);


-- Physical - Raw data converion
-- NOTE merge with package Data above ??

generic
  type TD is range <>; -- any signed integer type
  type TF is digits <>; -- any floating point type
			-- FIXME applies to IEEE_Float_nn as well ??
function Physical_Value_From_Int(BZERO : in TF; BSCALE : in TF; BLANK : in TD; Data : in TD) return TF;
-- for integer data in HDU




generic
  type TFp is digits <>; -- any floating point type
  type TFd is digits <>; -- any floating point type
			-- FIXME applies to IEEE_Float_nn as well ??
function Physical_Value_From_Float(BZERO : in TFp; BSCALE : in TFp; Data : in TFd) return TFd;
-- for Float data in HDU

end Data_Types;

