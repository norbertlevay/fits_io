

with Primary_Size_Info;
use  Primary_Size_Info;
-- Card_Block, Read_Control &
-- HDU_Size_Info_Type needed


package Ext_Strict is

	procedure Reset_State; 
        function  Next
                (BlockNum  : in Positive;
                 CardBlock : in Card_Block) return Read_Control;


	  function  Get return HDU_Size_Info_Type;


end Ext_Strict;


