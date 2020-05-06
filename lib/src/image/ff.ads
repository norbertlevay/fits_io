
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed

with Physical;
with Linear_Conv; use Linear_Conv;


generic
type Tout is digits <>;
type Tout_Arr is array (Positive_Count range <>) of Tout;
type Tcalc is digits <>; -- scaling always in Floats
type Tin  is digits <>;
ToutNaN : Tout;
package FF is


function FF_LinConv is new Linear_Conv.FF(Tin, Tcalc, Tout, ToutNaN);
package Phys is new Physical(Tout, Tout_Arr, Tcalc, Tin, FF_LinConv);


end FF;

