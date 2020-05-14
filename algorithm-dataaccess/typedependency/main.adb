
with V3_Types;  use V3_Types;
with Physical;

with Pool; use Pool;

procedure main
is


package F64F64 is new Physical(Float_64, Float_64);
package F32F32 is new Physical(Float_32, Float_32);
package F32I16 is new Physical(Float_32, Integer_16);
package F64I16 is new Physical(Float_64, Integer_16);

begin

F64F64.Read_Array(1);
F32F32.Read_Array(1);
F64I16.Read_Array(1);
F32I16.Read_Array(1);



end main;
