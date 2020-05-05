


with Physical;
with Linear_Conv; use Linear_Conv;


generic
type Tout is digits <>;
type Tin  is digits <>;
package FF is


function FF_LinConv is new Linear_Conv.FF(Tin, Tout, Tout);
package Phys is new Physical(Tout, Tout, Tin, FF_LinConv);


end FF;

