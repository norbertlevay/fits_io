

-- pool for instantiating directly image/Physical_Read


with V3_Types; use V3_Types;
with Value_Impl; use Value_Impl;



package V3_Pool_Scaling is


-- Tf -> F

 function Init_UOut is new Init_UOut_Tf2F(Float_64, Float_64, F64NaN);
 function Init_UOut is new Init_UOut_Tf2F(Float_32, Float_64, F64NaN);
 function Init_UOut is new Init_UOut_Tf2F(Integer_64, Float_64, F64NaN);
 function Init_UOut is new Init_UOut_Tf2F(Integer_32, Float_64, F64NaN);
 function Init_UOut is new Init_UOut_Tf2F(Integer_16, Float_64, F64NaN);
 function Init_UOut is new Init_UOut_Tf2F(Unsigned_8, Float_64, F64NaN);

 function Init_UOut is new Init_UOut_Tf2F(Float_64, Float_32, F32NaN);
 function Init_UOut is new Init_UOut_Tf2F(Float_32, Float_32, F32NaN);
 function Init_UOut is new Init_UOut_Tf2F(Integer_64, Float_32, F32NaN);
 function Init_UOut is new Init_UOut_Tf2F(Integer_32, Float_32, F32NaN);
 function Init_UOut is new Init_UOut_Tf2F(Integer_16, Float_32, F32NaN);
 function Init_UOut is new Init_UOut_Tf2F(Unsigned_8, Float_32, F32NaN);

-- F -> UI

 function Init_UOut is new Init_UOut_F2UI(Float_64, Integer_64);
 function Init_UOut is new Init_UOut_F2UI(Float_64, Integer_32);
 function Init_UOut is new Init_UOut_F2UI(Float_64, Integer_16);
 function Init_UOut is new Init_UOut_F2UI(Float_64, Unsigned_8);

 function Init_UOut is new Init_UOut_F2UI(Float_32, Integer_64);
 function Init_UOut is new Init_UOut_F2UI(Float_32, Integer_32);
 function Init_UOut is new Init_UOut_F2UI(Float_32, Integer_16);
 function Init_UOut is new Init_UOut_F2UI(Float_32, Unsigned_8);

-- UI -> UI

 function Init_UOut is new Init_UOut_UI2UI(Integer_64, Integer_64);
 function Init_UOut is new Init_UOut_UI2UI(Integer_64, Integer_32);
 function Init_UOut is new Init_UOut_UI2UI(Integer_64, Integer_16);
 function Init_UOut is new Init_UOut_UI2UI(Integer_64, Unsigned_8);

 function Init_UOut is new Init_UOut_UI2UI(Integer_32, Integer_64);
 function Init_UOut is new Init_UOut_UI2UI(Integer_32, Integer_32);
 function Init_UOut is new Init_UOut_UI2UI(Integer_32, Integer_16);
 function Init_UOut is new Init_UOut_UI2UI(Integer_32, Unsigned_8);

 function Init_UOut is new Init_UOut_UI2UI(Integer_16, Integer_64);
 function Init_UOut is new Init_UOut_UI2UI(Integer_16, Integer_32);
 function Init_UOut is new Init_UOut_UI2UI(Integer_16, Integer_16);
 function Init_UOut is new Init_UOut_UI2UI(Integer_16, Unsigned_8);

 function Init_UOut is new Init_UOut_UI2UI(Unsigned_8, Integer_64);
 function Init_UOut is new Init_UOut_UI2UI(Unsigned_8, Integer_32);
 function Init_UOut is new Init_UOut_UI2UI(Unsigned_8, Integer_16);
 function Init_UOut is new Init_UOut_UI2UI(Unsigned_8, Unsigned_8);




-- Scaling : check for Undef

 function Is_Undef is new Is_Undef_Floats(Float_64);
 function Is_Undef is new Is_Undef_Floats(Float_32);
 function Is_Undef is new Is_Undef_Ints(Integer_64);
 function Is_Undef is new Is_Undef_Ints(Integer_32);
 function Is_Undef is new Is_Undef_Ints(Integer_16);
 function Is_Undef is new Is_Undef_Ints(Unsigned_8);


end V3_Pool_Scaling;

