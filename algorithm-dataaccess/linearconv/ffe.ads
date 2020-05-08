
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed

with Physical;
with Linear_Conv; use Linear_Conv;


package FFe is


generic
type Tout is digits <>;
type Tout_Arr is array (Positive_Count range <>) of Tout;
ToutNaN : Tout;
package Out_Type is end;
-- helps only as logical grouping





generic
type Tout is digits <>;
type Tout_Arr is array (Positive_Count range <>) of Tout;
ToutNaN : Tout;
type Tcalc is digits <>; -- scaling always in Floats
type Tin  is digits <>;
package FF is
function FF_LinConv is new Linear_Conv.FF(Tin, Tcalc, Tout, ToutNaN);
package Phys is new Physical(Tout, Tout_Arr, Tcalc, Tin, FF_LinConv);
end FF;

end FFe;

