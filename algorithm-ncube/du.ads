
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with Mandatory; use Mandatory; -- NAXIS_Arr needed

-- NOTE
-- DU (DataUnit) implies Raw data (conversion to Physical in other modules)

-- offer two methods:
-- * Read/Write by buffers/Blocks
-- * Read/Write by Planes
-- both implement sequential access

package DU is

  package SIO renames Ada.Streams.Stream_IO;

-- both DU.Create and DU.Read require File Index set at first data in DU


-- access by Blocks

  procedure Coordinates
    (NAXISn : in NAXIS_Arr; 
    BlockNum : in Positive_Count; 
    IndexInBlock : in Positive; 
    Coord : out NAXIS_Arr) is null;
  -- util can be called within callbacks to determine coordinates of any point if needed
  -- In callbacks below count Blocks and call above Coordinates() to determine
  -- coord of any data element in Block

 -- all 4 implementation do sequential access to DU, so they 
 -- guarantee for the Data()-callbacks:
    -- each Block/Plane s accessed only once
    -- each Block/Plane of the DU will be accessed

generic
  type T is private;
  type T_Arr is array (Positive_Count range <>) of T;
  with procedure Data(Block : out T_Arr);
procedure Write
  (File : SIO.File_Type;
  NAXISn : in NAXIS_Arr);
  -- implemented as write-by-blocks
  -- Write in OUT Mode cuts the file
  -- pre-condition: File must have Mode = OUT or APPEND


generic
  type T is private;
  type T_Arr is array (Positive_Count range <>) of T;
  with procedure Data(Block : in T_Arr);
procedure Read
  (File : SIO.File_Type;
  NAXISn : in NAXIS_Arr);



-- access by Planes

-- NAXISn divided by i <= N=NAXIS index into
-- NAXISi  : NAXIS(1 .. i) - the Plane itself
-- NAXISin : NAXIS(i+1 .. N) - coordinates of one plane

-- similarlrly as in access by blocks:
-- in callbacks count number of planes and call
-- Cooridante(PlaneNum, NAXISi) -> Coord((i+1)..N) of given Plane
  procedure Plane_Coordinates
    (NAXISin : in NAXIS_Arr;
    PlaneNum : in Positive_Count; 
    Coord : out NAXIS_Arr) is null;


generic
  type T is private;
  type T_Arr is array (Positive_Count range <>) of T;
  with procedure Data(Plane : out T_Arr);
procedure Write_Planes
  (File : SIO.File_Type;
  NAXISn : in NAXIS_Arr;
  I : in Integer); -- 1 .. 999


generic
  type T is private;
  type T_Arr is array (Positive_Count range <>) of T;
  with procedure Data(Plane : in T_Arr);
procedure Read_Planes
  (File : SIO.File_Type;
  NAXISn : in NAXIS_Arr;
  I : in Integer); -- 1 .. 999
  -- implemented as read-by-planes


end DU;

