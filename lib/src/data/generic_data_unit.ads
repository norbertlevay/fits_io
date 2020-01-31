
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

-- sequential access

generic
  with procedure Element (V : in T);
procedure Read_Array_Values(F : SIO.File_Type; DUSize : in Positive);
-- call Physical_Value in Element to access physical values






end Generic_Data_Unit;




