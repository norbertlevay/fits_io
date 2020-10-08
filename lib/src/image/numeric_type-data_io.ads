
with Ada.Streams.Stream_IO;
with Mandatory; use Mandatory; -- NAXIS_Arr needed


generic
package Numeric_Type.Data_IO is

package SIO renames Ada.Streams.Stream_IO;



procedure Read (F : SIO.File_Type; A : out Float_Arr);
procedure Write(F : SIO.File_Type; A : in  Float_Arr);



-- Data Unit access


generic
  with procedure Elem(E : in Float);
procedure Read_Data_Unit
  (F : SIO.File_Type;
  NAXISn : in NAXIS_Arr);



generic
  with procedure Elem(E : out Float);
procedure Write_Data_Unit
  (F : SIO.File_Type;
  NAXISn : in NAXIS_Arr);


end Numeric_Type.Data_IO;

