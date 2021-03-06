
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

with FITS_IO; use FITS_IO;

with Ada.Streams.Stream_IO;

generic
  type T is private;
package Raw is

 package SIO renames Ada.Streams.Stream_IO;


 type T_Arr is array (Positive_Count range <>) of T;

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
    NAXISn  : in NAXIS_Array;
    First   : in NAXIS_Array;
    VolumeSize : in NAXIS_Array;
    --Last    : in NAXIS_Array;
    Volume  : out T_Arr);


 procedure Write_Volume
   (File : SIO.File_Type;
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Array;
    First   : in NAXIS_Array;
    VolumeSize : in NAXIS_Array;
    Volume     : in T_Arr);


end Raw;

