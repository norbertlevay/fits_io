
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed

with Physical;
with Linear_Conv; use Linear_Conv;


generic
type Tout is digits <>;
type Tout_Arr is array (Positive_Count range <>) of Tout;
type Tin  is digits <>;
package FF is


function FF_LinConv is new Linear_Conv.FF(Tin, Tout, Tout);
package Phys is new Physical(Tout, Tout_Arr, Tout, Tin, FF_LinConv);


end FF;

