
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Mandatory; use Mandatory; -- NAXIS_Arr needed


generic
package Numeric_Type.Stream is

package SIO renames Ada.Streams.Stream_IO;



procedure Read (S : SIO.Stream_Access; A : out Float_Arr);
procedure Write(S : SIO.Stream_Access; A : in  Float_Arr);



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


end Numeric_Type.Stream;

