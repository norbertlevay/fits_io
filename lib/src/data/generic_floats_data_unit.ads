
with Ada.Streams.Stream_IO;

with Generic_Data_Unit;

generic
  type Tin  is digits <>;
  type Tout is digits <>;
  with package Floats_Data_Unit is new Generic_Data_Unit(T => Tin);
  -- with function "+" (R : in Tin) return Tout is <>;
  -- conversion operator; down-conversions like Float64->Float_32 loose precision
package Generic_Floats_Data_Unit is

 package FDU renames Floats_Data_Unit;

 package SIO renames Ada.Streams.Stream_IO;
 use SIO;

 


-- provides converted physical float values

 generic
  with procedure Element_Value(V : in Tout);
  with procedure Undefined_Value is null;
 procedure Read_Checked_Floats
		(F : SIO.File_Type;
		Length : in Positive;
		BZERO  : in Tout;
		BSCALE : in Tout);

end Generic_Floats_Data_Unit;

