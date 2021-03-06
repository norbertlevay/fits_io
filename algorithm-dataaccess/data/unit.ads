
 with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;


-- NOTE data presents 3 orthogonal problems:
-- Positioning by linear-index (data/unit)  vs coordintes (NCube)
-- Access: sequential access by ICube's (I<N), random access of sub-Cube (maxi-coord < NAXISi)
-- data types (template programming, deferred to user inerface)



package Unit is

package SIO renames Ada.Streams.Stream_IO;



-- sequential access

generic
  type T is private;
  type T_Arr is array (Positive_Count range <>) of T;
procedure Read_Array_From_Current_Block
   (F : SIO.File_Type;
    Values : out T_Arr;
    First  : in Positive := 1); -- index within current block
-- reads i-dimensional sub-cube if:
-- First=1 & T_Arr'Length=NAXISi*...*NAXIS1
-- repeat call (NAXISn*...NAXISj)-times (j=i+1) to read all DU sequentially



-- random access

generic
  type T is private;
  type T_Arr is array (Positive_Count range <>) of T;
procedure Read_Array
  (F : SIO.File_Type;
   DUStart : in Positive_Count;      -- index in File of first Data-unit block
   First   : in Positive_Count := 1; -- index in DU of first data element to be read
   Values  : out T_Arr);




generic
  type T is private;
  type T_Arr is array (Positive_Count range <>) of T;
procedure Write_Array
   (F : SIO.File_Type;
   DUStart : in Positive_Count;       -- index in File of first Data-unit block
   First   : in Positive_Count := 1;  -- index in DU of first data element to be written
   Values  : in T_Arr);

end Unit;

