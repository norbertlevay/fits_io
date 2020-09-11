
-- data type used in DataUnit

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;-- Positive_Count needed$

generic
  type Tm is private;   -- type in memory
  type Tm_Arr is array (Positive_Count range <>) of Tm; 
  type Tc is digits <>; -- type in which scaling is calculated
package DU_Type is
end DU_Type;
