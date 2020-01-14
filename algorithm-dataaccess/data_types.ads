-- TODO
-- Physical_Value type Integer: implement Signed-Unsigned conversion

with Interfaces;	use Interfaces;
with Ada.Streams;

with Generic_Data_Types;
with Generic_Value;


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

package UInt8 is new Generic_Data_Types(T => Unsigned_8);
package Int16 is new Generic_Data_Types(T => Integer_16);
package Int32 is new Generic_Data_Types(T => Integer_32);
package Int64 is new Generic_Data_Types(T => Integer_64);

package Float32 is new Generic_Data_Types(T => Float_32);
package Float64 is new Generic_Data_Types(T => Float_64);





-- 3, Physical - Raw data converion


-- from Int data:

function Physical_Value is  
        new Generic_Value.Physical_From_Int(TF => Float_32, TD => Integer_32);
function Physical_Value is  
        new Generic_Value.Physical_From_Int(TF => Float_32, TD => Integer_16);
function Physical_Value is  
        new Generic_Value.Physical_From_Int(TF => Float_64, TD => Integer_32);
function Physical_Value is  
        new Generic_Value.Physical_From_Int(TF => Float_64, TD => Integer_16);

-- from Float data:

function Physical_Value is  
        new Generic_Value.Physical_From_Float(TF => Float_32);
function Physical_Value is  
        new Generic_Value.Physical_From_Float(TF => Float_64);




end Data_Types;

