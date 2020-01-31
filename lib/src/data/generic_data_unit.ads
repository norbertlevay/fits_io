
-- Data Unit resides in FITS File
-- File access needed

-- this module contains funcs operating on all Data Unit
-- funcs change File_Index and many funcs expect File_Index positioned at DU start

with Ada.Streams.Stream_IO;

generic
  type T is private; -- Data Block def
  type TF is digits <>;  -- Phys val conversion
  BZERO  : in out TF;
  BSCALE : in out TF;
  with function To_TF(P : in T) return TF; -- Phys Val conversion
package Generic_Data_Unit is

 package SIO renames Ada.Streams.Stream_IO;
 use SIO;

 function Physical_Value(Va : in T) return TF;

-- generic
--  type TInt is range <>;
--  BLANK : TInt;
-- function Physical_Value(Va : in TInt) return TF;

-- Sequential access

generic
  with procedure Element (V : in T);
procedure Read_Array_Values(F : SIO.File_Type; DUSize : in Positive);
-- call Physical_Value in Element to access physical values
-- FIXME consider: new param DUStart so forcing caller to make 
-- sure that File.Index is set to start of DU





-- Random access

type T_Arr is array (Integer range <>) of T;
procedure Read_Array_Value(F : SIO.File_Type;
		DUStart : in SIO.Positive_Count; 
		Offset  : in Positive;
		Count   : in Positive :=1;
		Value  : out T_Arr) is null;
-- FIXME not implemented


end Generic_Data_Unit;




