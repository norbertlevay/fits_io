-- TODO
-- Physical_Value type Integer: implement Signed-Unsigned conversion

with Interfaces;	use Interfaces;
with Ada.Streams;

with Generic_Data_Types; use Generic_Data_Types;

package Data_Types is

   -- data types as of FITS Standard version 3

   type Unsigned_8 is new Interfaces.Unsigned_8;
   type Integer_16 is new Interfaces.Integer_16;
   type Integer_32 is new Interfaces.Integer_32;
   type Integer_64 is new Interfaces.Integer_64;
   type Float_32   is new Interfaces.IEEE_Float_32;
   type Float_64   is new Interfaces.IEEE_Float_64;


   -- 1, Endianness

   procedure Int32_Read_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : out Integer_32 );

   procedure Int32_Write_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : in Integer_32 );

   procedure Float32_Read_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : out Float_32 );

   procedure Float32_Write_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : in Float_32 );

   for Integer_32'Read  use Int32_Read_BigEndian;
   for Integer_32'Write use Int32_Write_BigEndian;

   for Float_32'Read  use Float32_Read_BigEndian;
   for Float_32'Write use Float32_Write_BigEndian;




-- 2, Data Block definitions

package Data_UInt8 is new Data(T => Unsigned_8);
package Data_Int16 is new Data(T => Integer_16);
package Data_Int32 is new Data(T => Integer_32);
package Data_Int64 is new Data(T => Integer_64);

package Data_Float32 is new Data(T => Float_32);
package Data_Float64 is new Data(T => Float_64);





-- 3, Physical - Raw data converion


-- from Int data:

function Physical_Value is  
        new Physical_Value_From_Int(TF => Float_32, TD => Integer_32);
function Physical_Value is  
        new Physical_Value_From_Int(TF => Float_32, TD => Integer_16);
function Physical_Value is  
        new Physical_Value_From_Int(TF => Float_64, TD => Integer_32);
function Physical_Value is  
        new Physical_Value_From_Int(TF => Float_64, TD => Integer_16);

-- from Float data:

function Physical_Value is  
        new Physical_Value_From_Float(TFp => Float_32, TFd => Float_64);
function Physical_Value is  
        new Physical_Value_From_Float(TFp => Float_64, TFd => Float_32);
function Physical_Value is  
        new Physical_Value_From_Float(TFp => Float_32, TFd => Float_32);
function Physical_Value is  
        new Physical_Value_From_Float(TFp => Float_64, TFd => Float_64);
-- FIXME are above two needed? : 
-- should we write generic with one type and so without conversion ? 
-- is compiler smart enough to recognize no need for converions?





end Data_Types;

