

-- this goes with Tc_Physical


with V3_Types; use V3_Types;
with Conv_Impl; use Conv_Impl;
with Pool_V3Type_Convs; use Pool_V3Type_Convs;

package Tc_Pool is


function Is_Undef is new Is_Undef_NaN(Float_32);
function Is_Undef is new Is_Undef_BLANK(Integer_16);

function Do_Undef is new Do_Undef_BLANK(Integer_16);
function Do_Undef is new Do_Undef_BLANK(Float_32);
function Do_Undef is new Do_Undef_BLANK(Float_64);

function ConvIn  is new Conv_Pure(Float_64, Float_64);
function ConvOut is new Conv_Pure(Float_64, Float_64);

function ConvIn  is new Conv_Pure(Float_32, Float_32);
function ConvOut is new Conv_Pure(Float_32, Float_32);

function ConvOut is new Conv_Check_UndefIn(Integer_16, Float_32);
function ConvIn  is new Conv_Check_UndefIn(Integer_16, Float_64);

function ConvIn is new Conv_Check_UndefOut(Float_32, Integer_16);

end Tc_Pool;

