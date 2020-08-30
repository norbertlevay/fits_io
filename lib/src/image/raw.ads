
-- FIXME  later make T_Arr private
-- such record should also store: NAXISi BITPIX, Undef_Val/BLANK

-- FIXME all Read should be generator functions returning 
-- Plane or Volume arrays sized according to input params: 
-- for Plane add param NAXISi
-- for Volume there is Last - First (both NAXISn)

-- NOTE
-- Read_Plane:  needs size but not First:
-- * needs T_Arr of exact plane-size but does not calculate position of First 
-- * (assumes File_Index is at Plane boundary)
-- Read_Volume: needs First but not size:
-- * needs to calc position to First but size can be whatever
-- * (but not going beyond DU-end)


-- FIXME what Indexes should T_Arr have ?  (1..max or 'First .. 'Last)

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Mandatory; use Mandatory; -- NAXIS_Arr needed

generic
  type T is private;
  type T_Arr is array (Positive_Count range <>) of T;
package Raw is

 package SIO renames Ada.Streams.Stream_IO;


-- sequential access


 procedure Read_Array
   (F : SIO.File_Type;
    Data : out T_Arr);


 procedure Write_Array
   (F : SIO.File_Type;
   Data : in T_Arr);



-- random access


 procedure Read_Volume
   (File : SIO.File_Type;
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    Last    : in NAXIS_Arr;
    Volume  : out T_Arr);


 procedure Write_Volume
   (File : SIO.File_Type;
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    VolumeSize : in NAXIS_Arr;
    Volume     : in T_Arr);



-- access all Data Unit


 Dummy_Arr : T_Arr(1..1);
 -- FIXME this is to avoid Warning about Division by zero at instantiation with Floats
 -- because T'Size yields zero for Floats
 -- Fix: 'Size on type is minimum bits needed, 'Size on variable is actual number of bits used
subtype T_Data_Block is T_Arr(1 .. 2880/(Dummy_Arr(1)'Size/8));

generic
  with procedure Data(Block : in T_Data_Block);
procedure Read_Data_Unit
  (File : SIO.File_Type;
  NAXISn : in NAXIS_Arr);


generic
  T_DataPadding : T;
  with procedure Data(Block : out T_Data_Block);
procedure Write_Data_Unit
  (File : SIO.File_Type;
  NAXISn : in NAXIS_Arr);


generic
  with procedure Data_Elem(E : in T);
procedure Read_Data_Unit_By_Element
  (File : SIO.File_Type;
  NAXISn : in NAXIS_Arr);


end Raw;

