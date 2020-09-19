
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Mandatory; use Mandatory; -- NAXIS_Arr needed
with Image;

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





-- experimantal API

-- when instantiating Scaling caller has given two types
-- so when writing, one is the type used for file, and type in
-- param of Write func is the data in mem which caller wants to write

-- Scaling always transforms in direction from Tsrc -> Tdst
-- so:
-- Read   Tsrc = Tf   Tds = Tm
-- Write  Tsrc = Tm   Tds = Tf
-- E.g. caller needs different instant of Scaling for Reading and for Writing
-- FIXME above is no good; could one instantiona allow both read and write ?
-- e.g. we instantiate as Tf and Tm (and not Tsrc Tdst)
-- Then Scaling.Linear has to be reversed (A,B recalculated) and types swapped
-- OR
-- use two packages: Scaling.Stream_In Scaling.Stream_Out




generic
 with package ImDM is new Image(<>);
procedure Read_Array
    (F : SIO.File_Type;
    Im : in  ImDM.Data_Model; -- describes data stored in Tdst.Numeric_Arr
    Am : out Tdst.Numeric_Arr);

generic
 with package ImDM is new Image(<>);
procedure Write_Array
    (F : SIO.File_Type;
    Im : in ImDM.Data_Model;-- describes data stored in Tsrc.Numeric_Arr
    Am : in Tsrc.Numeric_Arr);




end Scaling.Streams;

