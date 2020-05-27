

with Optional;



generic
  type Tm is private;   -- type in memory
  type Tc is digits <>; -- type in which scaling is calculated
  type Tf is private;   -- type in fits-file
with procedure Header_Info
        (Cards : in Optional.Card_Arr;
        A : out Tm; B: out Tm; 
        BV : out Boolean; BLANK : out Tf) is <>;
package HInfo is


procedure Get
        (Cards : in Optional.Card_Arr;
        A : out Tm; B: out Tm; 
        BV : out Boolean; BLANK : out Tf);


end HInfo;

