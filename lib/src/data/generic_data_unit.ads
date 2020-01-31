
-- Data Unit resides in FITS File
-- File access needed

-- this module contains funcs operating on all Data Unit
-- funcs change File_Index and many funcs expect File_Index positioned at DU start

with Ada.Streams.Stream_IO;

generic
  type T is private; -- Data Block def
  type TF is digits <>;  -- Phys val conversion
  BZERO, BSCALE : in out TF;
  with function Conv_TF(P : in T) return TF; -- Phys Val conversion
package Generic_Data_Unit is

 package SIO renames Ada.Streams.Stream_IO;
 use SIO;


generic
  B_Min, B_Max : in TF; -- init values
  with function "<" (L : in TF; R : in TF) return Boolean;
  with function ">" (L : in TF; R : in TF) return Boolean;
 procedure MinMax(F : SIO.File_Type; DUSize : in Positive; Min : out TF; Max : out TF);


end Generic_Data_Unit;




