
-- NOTE: core must work in scope of all Ada-types (incl Interfaces)
-- NOTE: it is specialization like V3 which provides some type-subset (and hides Tf)

-- NOTE
-- caller has data Tm and can choose Tf (even if looses significant precision)
-- Write call will do necessary conversions and produce cards BZERO BSCALE:
-- Tm = Float_N -> Tf = Float_M
-- Tm = Float_N -> Tf = Int_M       yields: BZERO,BSCALE
-- Tm = Float_N -> Tf = UInt_M      yields: BZERO,BSCALE
-- Tm = Int_N  <-> Tf = Float_M     yields: BZERO,BSCALE
-- Tm = UInt_N <-> Tf = Float_M     yields: BZERO,BSCALE
-- Tm = Int_N  <-> Tf = UInt_M      yields: BZERO,BSCALE Tab.11


with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Mandatory; use Mandatory; -- NAXIS_Arr needed
with Optional; -- Card_Arr needed


generic
  type Tm is private;   -- type in memory
  type Tm_Arr is array (Positive_Count range <>) of Tm;
  type Tc is digits <>; -- type in which scaling is calculated
  type Tf is private;   -- type in fits-file

with function Init_UOut(UInValid : in Boolean; UIn : in Tm; UOutValid : in out Boolean; UOut : in out Tf) return Boolean is <>;

with function Is_Undef(V,U : Tf; UValid : Boolean) return Boolean is <>;
with function Is_Undef(V,U : Tm; UValid : Boolean) return Boolean is <>;

with function "+"(R : Tm) return Tc is <>;
with function "+"(R : Tc) return Tf is <>;
--with function To_V3Type(Arg : String) return Tc is <>;
with function To_V3Type(Arg : String) return Tf is <>;
with function To_V3Type(Arg : String) return Tm is <>;


package Physical_Write is

 package SIO renames Ada.Streams.Stream_IO;



     procedure Write_Array
         (F : SIO.File_Type;
         Data : in Tm_Arr;
         Undef_Value : in out Tm;
         Undef_Valid : in out Boolean;
         Cards : in Optional.Card_Arr);



end Physical_Write;

