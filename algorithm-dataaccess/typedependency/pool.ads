
with V3_Types; use V3_Types;
with Linear_Impl; use Linear_Impl;

package Pool is

procedure Header_Info is new From_Header(Float_64, Float_64);
procedure Header_Info is new From_Header(Float_32, Float_32);
procedure Header_Info is new From_Header(Integer_16, Float_32);
procedure Header_Info is new From_Header(Integer_16, Float_64);



function Linear is new Linear_Pure(Float_64,   Float_64);
function Linear is new Linear_Pure(Float_32,   Float_32);
function Linear is new Linear_Check_UndefIn(Integer_16, Float_32);
function Linear is new Linear_Check_UndefIn(Integer_16, Float_64);


end Pool;
