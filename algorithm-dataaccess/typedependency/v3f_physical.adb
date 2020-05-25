
with Ada.Text_IO;
with Linear_Impl; use Linear_Impl;

with Physical;
with Pool; use Pool;-- pulls in default funcs in F32/I16_Physical instantiation new Physical


package body V3f_Physical is

package TIO renames Ada.Text_IO;


procedure Read_Array(Dummy:Integer)
is

function Do_Undef is new Do_Undef_BLANK(Tm);
function Linear is new Linear_Pure(Float_32, Tm);
function Linear is new Linear_Check_UndefIn(Integer_16, Tm);
package F32_Physical is new Physical(Tm, Float_32);
package I16_Physical is new Physical(Tm, Integer_16);


begin
TIO.Put_Line("V3f_Physical::Read_Array");


    F32_Physical.Read_Array(Dummy);
    I16_Physical.Read_Array(Dummy);


end Read_Array;


end V3f_Physical;

