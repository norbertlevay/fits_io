

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed

with Physical_Private;
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
function Linear  is new Linear_Conv.FF(Tin, Tcalc, Tout, A, B, ToutNaN);
package Physical is new Physical_Private(Tout, Tout_Arr, Tcalc, Tin, Linear);
end FF;



generic
type Tout is digits <>;
type Tout_Arr is array (Positive_Count range <>) of Tout;
type Tcalc is digits <>;
type Tin is range <>;
A,B : in out Tcalc;
package FI is
function Linear  is new Linear_Conv.FI(Tin, Tcalc, Tout, A,B);
package Physical is new Physical_Private(Tout, Tout_Arr, Tcalc, Tin, Linear);
end FI;



end Image_Data;
