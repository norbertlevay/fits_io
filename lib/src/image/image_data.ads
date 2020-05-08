

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed

with Physical;
with Linear_Conv; use Linear_Conv;


package Image_Data is



generic
type Tout is digits <>;
type Tout_Arr is array (Positive_Count range <>) of Tout;
type Tcalc is digits <>; -- scaling always in Floats
type Tin  is digits <>;
A,B: in out Tcalc;
ToutNaN : Tout;
package FF is
function FF_LinConv is new Linear_Conv.FF(Tin, Tcalc, Tout, A, B, ToutNaN);
package Phys is new Physical(Tout, Tout_Arr, Tcalc, Tin, FF_LinConv);
end FF;



generic
type Tout is digits <>;
type Tout_Arr is array (Positive_Count range <>) of Tout;
type Tcalc is digits <>;
type Tin is range <>;
A,B : in out Tcalc;
package FI is
function FI_LinConv is new Linear_Conv.FI(Tin, Tcalc, Tout, A,B);
package Phys is new Physical(Tout, Tout_Arr, Tcalc, Tin, FI_LinConv);
end FI;



end Image_Data;
