

-- pool for instantiating directly image/Physical_Read


with V3_Types; use V3_Types;
with Linear_Impl; use Linear_Impl;
with Pool_V3Type_Convs; use Pool_V3Type_Convs;



package V3_Pool_Linear is
--                                            Tin         Tcalc     Tout
function Linear is new Linear_Pure         (Float_64,   Float_64, Float_64);
function Linear is new Linear_Pure         (Float_32,   Float_64, Float_64);
function Linear is new Linear_Check_UndefIn(Integer_64, Float_64, Float_64, F64NaN);
function Linear is new Linear_Check_UndefIn(Integer_32, Float_64, Float_64, F64NaN);
function Linear is new Linear_Check_UndefIn(Integer_16, Float_64, Float_64, F64NaN);
function Linear is new Linear_Check_UndefIn(Unsigned_8, Float_64, Float_64, F64NaN);

function Linear is new Linear_Pure (Float_32,   Float_32, Float_32);
function Linear is new Linear_Pure (Integer_64, Float_64, Integer_64);
function Linear is new Linear_Pure (Integer_32, Float_64, Integer_32);
function Linear is new Linear_Pure (Integer_16, Float_32, Integer_16);
function Linear is new Linear_Pure (Unsigned_8, Float_32, Unsigned_8);

-- only for test F->UI : we need BLANK for output if NaN encoutered
function Linear is new Linear_Pure           (Float_64, Float_64, Float_32);
function Linear is new Linear_Check_UndefOut (Float_64, Float_64, Integer_64, Integer_64'Last);
function Linear is new Linear_Check_UndefOut (Float_64, Float_64, Integer_32, Integer_32'Last);
function Linear is new Linear_Check_UndefOut (Float_64, Float_64, Integer_16, Integer_16'Last);
function Linear is new Linear_Check_UndefOut (Float_64, Float_64, Unsigned_8, Unsigned_8'Last);

function Linear is new Linear_Check_UndefOut (Float_32, Float_32, Integer_64, Integer_64'Last);
function Linear is new Linear_Check_UndefOut (Float_32, Float_32, Integer_32, Integer_32'Last);
function Linear is new Linear_Check_UndefOut (Float_32, Float_32, Integer_16, Integer_16'Last);
function Linear is new Linear_Check_UndefOut (Float_32, Float_32, Unsigned_8, Unsigned_8'Last);

function Linear is new Linear_Check_UndefOut(Float_32,   Float_64, Integer_64, Integer_64'Last);
function Linear is new Linear_Pure(Integer_32, Float_64, Integer_64);
function Linear is new Linear_Pure(Integer_16, Float_64, Integer_64);
function Linear is new Linear_Pure(Unsigned_8, Float_64, Integer_64);


end V3_Pool_Linear;

