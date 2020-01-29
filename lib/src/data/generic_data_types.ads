 
-- FIXME how-to: should refuse to instantiate for T if not in 2880 without gap
-- FIXME how-to: guarantee that Block-array is packed for any T


with Ada.Streams;

generic
  type T is private; -- any type of known size at compile time (definite and unlimited)
package Generic_Data_Types is

 -- 1, Data Block definition (always 2880 bytes)

 Block_Size : constant Positive := 2880*8;
 N : constant Positive := Block_Size / T'Size;
 type Block is array (Positive range 1 .. N) of T;

 generic
  with function "<" (L : in T; R : in T) return Boolean; 
 function Min(B : in Block; B_Min : in T) return T;

 generic
  with function ">" (L : in T; R : in T) return Boolean; 
 function Max(B : in Block; B_Max : in T) return T;


 -- 2, Endianness

private
 
-- FIXME consider: these have Stream_IO, should go to File; here only Revert_Bytes()
-- possible ?
   
   procedure T_Read_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : out Block );

   procedure T_Write_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : in Block );

  for Block'Read  use T_Read_BigEndian;
  for Block'Write use T_Write_BigEndian;

end Generic_Data_Types;

