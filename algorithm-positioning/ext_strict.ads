
with FITS;  use  FITS;
-- Card_Type HDU_Size_Info_Type needed

package Ext_Strict is

type Options_Type is
         (ALGORITHM_STRICT,      -- parsing Headers follows strictly FITS-Standard
          ALGORITHM_TOLERANT);   -- parsing Headers fails only if: 
                                 -- * essential key is missing
                                 -- * essential key is duplicate with different values (ambiguity) 


	procedure Configuration(Options : Options_Type) is null;
	procedure Reset_State; 
        function  Next (Pos  : in Positive; Card : in Card_Type) return Natural;
	  
	function  Get return HDU_Size_Info_Type;


end Ext_Strict;


