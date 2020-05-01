
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

-- NOTE
-- DU (DataUnit) implies Raw data (conversion to Physical in other modules)

-- offer two methods:
-- * Read/Write by buffers/Blocks
-- * Read/Write by Planes
-- both implement sequential access

package DU is

  package SIO renames Ada.Streams.Stream_IO;

-- both DU.Create and DU.Read require File Index set at first data in DU

generic
  type T is private;
  type T_Arr is array (Positive_Count range <>) of T;
  with procedure Data(First : in NAXIS_Arr; Block : out T_Arr);
procedure Write
  (File : SIO.File_Type;
  NAXISn : in NAXIS_Arr);
  -- implemented as write-by-blocks
  -- Write in OUT Mode cuts the file
  -- pre-condition: File must have Mode = OUT or APPEND



generic
  type T is private;
  type T_Arr is array (Positive_Count range <>) of T;
  with procedure Data(PlaneCoord: in NAXIS_Arr; Plane : in T_Arr);
procedure Read
  (File : SIO.File_Type;
  NAXISn : in NAXIS_Arr;
  I : in Integer); -- 1 .. 999
  -- implemented as read-by-planes


end DU;

