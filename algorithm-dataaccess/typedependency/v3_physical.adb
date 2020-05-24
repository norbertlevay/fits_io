
with Ada.Text_IO;


--generic
--type Tm is private;
--with F32_Physical is new Physical(Tm, Float_32);
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

