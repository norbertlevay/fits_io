
-- Data Unit resides in FITS File
-- File access needed

-- this module contains funcs operating on all Data Unit
-- funcs change File_Index and many funcs expect File_Index positioned at DU start

with Ada.Streams.Stream_IO;

generic
  type T is private; -- Data Block def
package Generic_Data_Unit is

 package SIO renames Ada.Streams.Stream_IO;
 use SIO;
 

-- Sequential access

generic
  with procedure Element (V : in T);
procedure Read_Array_Values(F : SIO.File_Type; DUSize : in Positive);
-- FIXME consider: new param DUStart so forcing caller to make 
-- sure that File.Index is set to start of DU


 generic
--  type Tin  is private;
  type Tout is private;
  BZERO  : in out Tout;
  BSCALE : in out Tout;
  with procedure Physical_Elem(V : in Tout);
  with function "+" (L, R : in Tout) return Tout;
  with function "*" (L, R : in Tout) return Tout;
  with function "+" (R : in T) return Tout;
 procedure Read_Physical_Values(F : SIO.File_Type; DUSize : in Positive);


 generic
--  type Tin  is private;
  type Tout is private;
  BZERO  : in out Tout;
  BSCALE : in out Tout;
  BLANK  : in out T;
  with procedure Physical_Elem(V : in Tout);
  with procedure Undefined_Value(V : in T);
  with function "+" (L, R : in Tout) return Tout;
  with function "*" (L, R : in Tout) return Tout;
  with function "+" (R : in T) return Tout;
 procedure Read_Checked_Physical_Values(F : SIO.File_Type; DUSize : in Positive);






-- Random access

type T_Arr is array (Integer range <>) of T;
procedure Read_Array_Value(F : SIO.File_Type;
		DUStart : in SIO.Positive_Count; 
		Offset  : in Positive;
		Count   : in Positive :=1;
		Value  : out T_Arr) is null;
-- FIXME not implemented


end Generic_Data_Unit;




