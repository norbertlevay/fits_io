with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Physical;

package physXY is


generic
type Tm is digits <>;       -- type in memory (data is returned to caller in this type)
type Tm_Arr is array (Positive_Count range <>) of Tm; 
type Tc is digits <>;     -- type in which scaling is calculated
type Tf is digits <>;       -- type in fits-file;
with function Linear(BZERO,BSCALE : in Tc; Vin : in Tf) return Tm is <>; 
with package Phys is new Physical(Tm,Tm_Arr,Tc,Tf,Linear);
package FF is end;


generic
type Tm is digits <>;       -- type in memory (data is returned to caller in this type)
type Tm_Arr is array (Positive_Count range <>) of Tm; 
type Tc is digits <>;     -- type in which scaling is calculated
type Tf is range <>;       -- type in fits-file;
with function Linear(BZERO,BSCALE : in Tc; Vin : in Tf) return Tm is <>; 
with package Phys is new Physical(Tm,Tm_Arr,Tc,Tf,Linear);
package FI is end;



end physXY;

