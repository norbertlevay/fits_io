-- TODO
-- Physical_Value type Integer: implement Signed-Unsigned conversion [FITS Tab 11]

with Ada.Streams;
with Ada.Streams.Stream_IO;

generic
  type T is private; -- any type of known size at compile time (definite and unlimited)
package Generic_Data_Types is

 package SIO renames Ada.Streams.Stream_IO;

 -- 1, Data Block definition (always 2880 bytes)

 Block_Size : constant Positive := 2880*8;
 N : constant Positive := Block_Size / T'Size;
 type Block is array (Positive range 1 .. N) of T;
 -- FIXME how-to: should refuse to instantiate for T if above division is not without reminder
 -- FIXME how-to: guarantee that array is packed for any T


 
-- 2, Endianness

private
    
   procedure T_Read_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : out Block );

   procedure T_Write_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : in Block );

  for Block'Read  use T_Read_BigEndian;
  for Block'Write use T_Write_BigEndian;

end Generic_Data_Types;

