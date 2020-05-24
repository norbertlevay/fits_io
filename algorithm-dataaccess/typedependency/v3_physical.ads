
with Physical;
with V3_Types; use V3_Types;
with Pool; use Pool;


generic
type Tm is private;
with package F32_Physical is new Physical(<>);
with package I16_Physical is new Physical(<>);
--with package F32_Physical is new Physical(Tm, Float_32);
package V3_Physical is


procedure Read_Array(Dummy:Integer);




end V3_Physical;

