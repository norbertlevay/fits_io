
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Mandatory; use Mandatory; -- NAXIS_Arr needed


generic
package Scaling.Streams is

package SIO renames Ada.Streams.Stream_IO;

-- Read: replaces source-array with Stream
procedure Linear(Ssrc : SIO.Stream_Access; Aout : out Tdst.Numeric_Arr);
-- Write: replaces destination-array with Stream
procedure Linear(Ain : in Tsrc.Numeric_Arr; Sdst : SIO.Stream_Access);


-- Data Unit access

-- src File --> Tdst
generic
  with procedure Elem(E : in Tdst.Numeric);
procedure Read_Data_Unit
  (F : SIO.File_Type;
  NAXISn : in NAXIS_Arr);

-- Tsrc --> dst is File
generic
  with procedure Elem(E : out Tsrc.Numeric);
procedure Write_Data_Unit
  (F : SIO.File_Type;
  NAXISn : in NAXIS_Arr);





end Scaling.Streams;

