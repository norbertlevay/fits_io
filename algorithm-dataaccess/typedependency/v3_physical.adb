
with Ada.Text_IO;


package body V3_Physical is

package TIO renames Ada.Text_IO;


procedure Read_Array(Dummy:Integer)
is
begin
TIO.Put_Line("V3_Physical::Read_Array");


    F32_Physical.Read_Array(Dummy);
    I16_Physical.Read_Array(Dummy);


end Read_Array;




end V3_Physical;

