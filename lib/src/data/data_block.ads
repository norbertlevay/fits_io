 
-- FIXME how-to: should refuse to instantiate for T if not in 2880 without gap


with Ada.Streams;

generic
  type T is private;
package Data_Block is

    -- 1, Data Block definition (always 2880 bytes)

    Block_Size : constant Positive := 2880*8;
    N : constant Positive := Block_Size / T'Size;
    type Block is array (Positive range 1 .. N) of T;
    pragma Pack (Block);

    function Get_Value(Blk : in Block; Index : in Positive) return T;

private

    -- 2, Endianness

   procedure T_Read_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : out Block );

   procedure T_Write_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : in Block );

  for Block'Read  use T_Read_BigEndian;
  for Block'Write use T_Write_BigEndian;

end Data_Block;

