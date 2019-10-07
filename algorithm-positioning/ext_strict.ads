

with Primary_Size_Info;
use  Primary_Size_Info;
-- Card_Block, Read_Control &
-- HDU_Size_Info_Type needed


package Ext_Strict is

	procedure Configuration(Options : Options_Type) is null;
	procedure Reset_State; 
        function  Next (Pos  : in Positive; Card : in Card_Type) return Natural;
	  
	function  Get return HDU_Size_Info_Type;


end Ext_Strict;


