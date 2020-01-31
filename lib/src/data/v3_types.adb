
package body V3_Types is

 package body UInt8 is
     function To_Signed(D : in Unsigned_8) return Integer_8
     is
     begin
	if(D >= 128)
	then
		return Integer_8(D - 128);
	else
		return -Integer_8(128 - D);
	end if;
     end To_Signed;
 end UInt8;

end V3_Types;
