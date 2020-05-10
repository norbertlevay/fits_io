

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed

with Physical_Private;
with Linear_Conv; use Linear_Conv;


package Image_Data is

 -- FF FI FU

generic
type Tmem is digits <>;
type Tmem_Arr is array (Positive_Count range <>) of Tmem;
type Tcalc is digits <>;
type Tfile  is digits <>;
A,B: in out Tcalc;
TmemNaN : Tmem;
TfileNaN : Tfile;
package FF is

    package Physical is new Physical_Private(Tmem, Tmem_Arr, Tcalc, Tfile);
    function Linear4R is new Linear_Conv.FF(Tfile, Tcalc, Tmem, A, B, TmemNaN);
    function Linear4W is new Linear_Conv.FF(Tmem, Tcalc, Tfile, A, B, TfileNaN);

    package Physical_In  is new Physical.Input(Linear4R);
    package Physical_Out is new Physical.Output(Linear4W);

end FF;



generic
type Tmem is digits <>;
type Tmem_Arr is array (Positive_Count range <>) of Tmem;
type Tcalc is digits <>;
type Tfile is range <>;
A,B : in out Tcalc;
package FI is

    package Physical is new Physical_Private(Tmem, Tmem_Arr, Tcalc, Tfile);
    function Linear4R  is new Linear_Conv.FI(Tfile, Tcalc, Tmem, A,B);
    function Linear4W  is new Linear_Conv.rF(Tmem, Tcalc, Tfile, A,B);

    package Physical_In  is new Physical.Input(Linear4R);
    package Physical_Out is new Physical.Output(Linear4W);

end FI;



generic
type Tmem is digits <>;
type Tmem_Arr is array (Positive_Count range <>) of Tmem;
type Tcalc is digits <>;
type Tfile is mod <>;
A,B : in out Tcalc;
package FU is

    package Physical is new Physical_Private(Tmem, Tmem_Arr, Tcalc, Tfile);
    function Linear4R  is new Linear_Conv.FU(Tfile, Tcalc, Tmem, A,B);
    function Linear4W  is new Linear_Conv.UF(Tmem, Tcalc, Tfile, A,B);

    package Physical_In  is new Physical.Input(Linear4R);
    package Physical_Out is new Physical.Output(Linear4W);

end FU;



 -- rF II IU

generic
type Tmem is range <>;
type Tmem_Arr is array (Positive_Count range <>) of Tmem;
type Tcalc is digits <>;
type Tfile  is digits <>;
A,B: in out Tcalc;
TmemNaN : Tmem;
TfileNaN : Tfile;
package rF is

    package Physical is new Physical_Private(Tmem, Tmem_Arr, Tcalc, Tfile);
    function Linear4R is new Linear_Conv.rF(Tfile, Tcalc, Tmem, A, B);
    function Linear4W is new Linear_Conv.FI(Tmem, Tcalc, Tfile, A, B);

    package Physical_In  is new Physical.Input(Linear4R);
    package Physical_Out is new Physical.Output(Linear4W);

end rF;



generic
type Tmem is range <>;
type Tmem_Arr is array (Positive_Count range <>) of Tmem;
type Tcalc is digits <>;
type Tfile is range <>;
A,B : in out Tcalc;
package II is

    package Physical is new Physical_Private(Tmem, Tmem_Arr, Tcalc, Tfile);
    function Linear4R  is new Linear_Conv.II(Tfile, Tcalc, Tmem, A,B);
    function Linear4W  is new Linear_Conv.II(Tmem, Tcalc, Tfile, A,B);

    package Physical_In  is new Physical.Input(Linear4R);
    package Physical_Out is new Physical.Output(Linear4W);

end II;



generic
type Tmem is range <>;
type Tmem_Arr is array (Positive_Count range <>) of Tmem;
type Tcalc is digits <>;
type Tfile is mod <>;
A,B : in out Tcalc;
package IU is

    package Physical is new Physical_Private(Tmem, Tmem_Arr, Tcalc, Tfile);
    function Linear4R  is new Linear_Conv.IU(Tfile, Tcalc, Tmem, A,B);
    function Linear4W  is new Linear_Conv.UI(Tmem, Tcalc, Tfile, A,B);

    package Physical_In  is new Physical.Input(Linear4R);
    package Physical_Out is new Physical.Output(Linear4W);

end IU;




end Image_Data;
