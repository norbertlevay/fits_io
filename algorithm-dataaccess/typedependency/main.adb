
with V3_Types;  use V3_Types;
with Physical;

with Pool; use Pool;

with Header;

procedure main
is


package F64F64 is new Physical(Float_64, Float_64);
package F32F32 is new Physical(Float_32, Float_32);
package F32I16 is new Physical(Float_32, Integer_16);
package F64I16 is new Physical(Float_64, Integer_16);

BV  : Boolean;
HInf : Header.Linear_Conv_Rec;

F64A, F64B : Float_64;
F32A, F32B : Float_32;

F64UndefIn, F64UndefOut : Float_64;
F32UndefIn, F32UndefOut : Float_32;
I16UndefIn : Integer_16;

begin

F64F64.Conv_Header(HInf, F64A,F64B, BV, F64UndefIn, F64UndefOut);
F32F32.Conv_Header(HInf, F32A,F32B, BV, F32UndefIn, F32UndefOut);
F64I16.Conv_Header(HInf, F64A,F64B, BV, I16UndefIn, F64UndefOut);
F32I16.Conv_Header(HInf, F32A,F32B, BV, I16UndefIn, F32UndefOut);

F64F64.Read_Array(1);
F32F32.Read_Array(1);
F64I16.Read_Array(1);
F32I16.Read_Array(1);



end main;
