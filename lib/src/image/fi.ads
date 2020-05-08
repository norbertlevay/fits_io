
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed

with Physical;
with Linear_Conv; use Linear_Conv;


generic
type Tout is digits <>;
type Tout_Arr is array (Positive_Count range <>) of Tout;
type Tcalc is digits <>;
type Tin is range <>;
A,B : Tcalc;
package FI is


function FI_LinConv is new Linear_Conv.FI(Tin, Tcalc, Tout, A,B);
package Phys is new Physical(Tout, Tout_Arr, Tcalc, Tin, FI_LinConv);


-- instantiate and access:
--
-- package F32I16 is new FI(Float_32, Integer_16);
--
-- F32I16.Phys.Read_Array(F, A, B, Data);







--generic
--type Tout is digits <>;
--type Tin is range <>;
--with function FI_Linear(A,B : Tout; Vin: Tin) return Tout;
--with package FI_Physical is new Physical(Tout,Tout,Tin,FI_Linear);
--package fipackage is end fipackage;



end FI;
