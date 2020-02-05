
-- Data Unit resides in FITS File
-- File access needed

-- For Read_<>() funcs:
-- FIXME consider: new param DUStart so forcing caller to make
-- sure that File.Index is set to start of DU
-- FIXME consider generlize to read some array from current 
-- File_Index position (implements Random Access) and 
-- specialize read array of size DUSize from DUStart (implements Sequential Access)


with Ada.Streams.Stream_IO;

generic
  type T is private;
package Generic_Data_Unit is

 package SIO renames Ada.Streams.Stream_IO;
 use SIO;

-- raw array data values in file (Varr)

generic
  with procedure Element (V : in T);
procedure Read_Array_Values(F : SIO.File_Type; DUSize : in Positive);


-- Conversion to physical values
-- Vphys = BZERO + BSCALE * Varr
 
-- converted physical integer values 

 generic
  type Tout is private;
  with function "+" (L, R : in Tout) return Tout is <>;
  with function "*" (L, R : in Tout) return Tout is <>;
  with function "+" (R : in T) return Tout is <>;
 package Physical is

 generic
  with procedure Element_Value(V : in Tout);
 procedure Read_Values
		(F : SIO.File_Type; 
		DUSize : in Positive;
		BZERO  : in Tout;
		BSCALE : in Tout);


-- FIXME func-name _Integers() does not fit Tout type spec
-- as below for Floats is ok
 generic
  with procedure Element_Value(V : in Tout);
  with procedure Undefined_Value is null;
 procedure Read_Checked_Integers
		(F : SIO.File_Type;
		DUSize : in Positive;
		BZERO  : in Tout;
		BSCALE : in Tout;
		BLANK  : in T);

end Physical;


-- Random access

type T_Arr is array (Integer range <>) of T;
procedure Read_Array_Value(F : SIO.File_Type;
		DUStart : in SIO.Positive_Count; 
		Offset  : in Positive;
		Count   : in Positive :=1;
		Value  : out T_Arr) is null;
-- FIXME not implemented


end Generic_Data_Unit;




