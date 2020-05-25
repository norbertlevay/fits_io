
with V3_Types; use V3_Types;
with Linear_Impl; use Linear_Impl;
with Pool_From_String; use Pool_From_String;
with Pool_V3Type_Convs; use Pool_V3Type_Convs;

package Pool is

procedure AB_Header_Info is new AB_From_Header(Float_64);
procedure AB_Header_Info is new AB_From_Header(Float_32);
procedure AB_Header_Info is new AB_From_Header(Integer_16);

procedure BLANK_Header_Info is new BLANK_From_Header(Float_64);
procedure BLANK_Header_Info is new BLANK_From_Header(Float_32);
procedure BLANK_Header_Info is new BLANK_From_Header(Integer_16);



function Is_Undef is new Is_Undef_NaN(Float_32);
function Is_Undef is new Is_Undef_BLANK(Integer_16);

function Do_Undef is new Do_Undef_BLANK(Integer_16);
function Do_Undef is new Do_Undef_BLANK(Float_32);
function Do_Undef is new Do_Undef_BLANK(Float_64);

function Linear is new Linear_Pure(Float_64, Float_64);
function Linear is new Linear_Pure(Float_32, Float_32);
function Linear is new Linear_Check_UndefIn(Integer_16, Float_32);
function Linear is new Linear_Check_UndefIn(Integer_16, Float_64);

function Linear is new Linear_Check_UndefOut(Float_32, Integer_16);

end Pool;

