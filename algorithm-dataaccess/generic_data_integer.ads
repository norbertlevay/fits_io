-- TODO
-- Physical_Value type Integer: implement Signed-Unsigned conversion [FITS Tab 11]

with Ada.Streams;

generic
  type T is range <>; -- any signed integer type
package Generic_Data_Integer is

 -- 1, Data Block definition (always 2880 bytes)

 Block_Size : constant Positive := 2880*8;
 N : constant Positive := Block_Size / T'Size;
 type Block is array (Positive range 1 .. N) of T;
 -- FIXME how-to: should refuse to instantiate for T if above division is not without reminder
 -- FIXME how-to: guarantee that array is packed for any T



-- 2, Endianness

   procedure Revert_Bytes( Data : in out T );
	-- Endian on data element level


-- 3, Phys-Arr value conversions

 generic
   type TF is digits <>; -- any floating point type
 function Physical(BZERO : in TF; BSCALE : in TF; BLANK : in T; Data : in T) return TF;



private
	
   -- Endianness by Block	

   procedure T_Read_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : out Block );

   procedure T_Write_BigEndian
                (S    : access Ada.Streams.Root_Stream_Type'Class;
                 Data : in Block );

  for Block'Read  use T_Read_BigEndian;
  for Block'Write use T_Write_BigEndian;

end Generic_Data_Integer;

