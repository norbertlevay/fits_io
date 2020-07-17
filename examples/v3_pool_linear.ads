

-- pool for instantiating directly image/Physical_Read


with V3_Types; use V3_Types;
with Linear_Impl; use Linear_Impl;
with Pool_V3Type_Convs; use Pool_V3Type_Convs;



package V3_Pool_Linear is

function Linear is new Linear_Pure(Float_64, Float_64, Float_64);
function Linear is new Linear_Pure(Float_32, Float_64,  Float_64);
function Linear is new Linear_Check_UndefIn(Integer_64, Float_64, Float_64);
function Linear is new Linear_Check_UndefIn(Integer_32, Float_64, Float_64);
function Linear is new Linear_Check_UndefIn(Integer_16, Float_64, Float_64);
function Linear is new Linear_Check_UndefIn(Unsigned_8, Float_64, Float_64);

end V3_Pool_Linear;
