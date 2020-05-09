

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
TinNaN : Tin;
package FF is

    package Physical is new Physical_Private(Tout, Tout_Arr, Tcalc, Tin);
    function Linear4R  is new Linear_Conv.FF4R(Tin, Tcalc, Tout, A, B, ToutNaN);
    function Linear4W  is new Linear_Conv.FF4W(Tin, Tcalc, Tout, A, B, TinNaN);

    package Physical_In  is new Physical.Input(Linear4R);
    package Physical_Out is new Physical.Output(Linear4W);

end FF;



generic
type Tout is digits <>;
type Tout_Arr is array (Positive_Count range <>) of Tout;
type Tcalc is digits <>;
type Tin is range <>;
A,B : in out Tcalc;
package FI is

    package Physical is new Physical_Private(Tout, Tout_Arr, Tcalc, Tin);
    function Linear4R  is new Linear_Conv.FI4R(Tin, Tcalc, Tout, A,B);
    function Linear4W  is new Linear_Conv.FI4W(Tin, Tcalc, Tout, A,B);

    package Physical_In  is new Physical.Input(Linear4R);
    package Physical_Out is new Physical.Output(Linear4W);

end FI;



end Image_Data;
