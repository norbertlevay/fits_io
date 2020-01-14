-- TODO
-- Physical_Value type Integer: implement Signed-Unsigned conversion [FITS Tab 11]


package Generic_Data_Types is

-- 1, Endianness

   generic
     type Data_Type is private;
   procedure Revert_Bytes( Data : in out Data_Type );



-- 2, Data Block definition (always 2880 bytes)


generic
        type T is private;
package Data is
Block_Size : constant Positive := 2880*8;
N : constant Positive := Block_Size / T'Size;
-- FIXME how-to: should refuse to instantiate for T if above division is not without reminder
-- FIXME how-to: guarantee that array is packed for any T
type Block is array (Positive range 1 .. N) of T;
end Data;



-- 3, 3, 3, Physical-Array data converions


-- A, [FITS Eq(3)] PhysVal type Float: below is Int-Float Float-Float conversions
generic
  type TD is range <>;  -- any signed integer type
  type TF is digits <>; -- any floating point type
function Physical_Value_From_Int(BZERO : in TF; BSCALE : in TF; BLANK : in TD; Data : in TD)
	return TF;
-- when HDU-array type is Integer

generic
  type TFp is digits <>; -- any floating point type
  type TFd is digits <>; -- any floating point type
function Physical_Value_From_Float(BZERO : in TFp; BSCALE : in TFp; Data : in TFd)
	return TFd;
-- whenHDU-array type is Float


-- B, [FITS Tab 11] PhysVal type Integer: Integer-data signed-unsigned converions

-- FIXME not yet implemented

end Generic_Data_Types;

