


with FITS; use FITS;

with Primary_Size_Info; use Primary_Size_Info;
with Ext_Strict;        use Ext_Strict;


package Interpret 
is
        function  Get(MandVals : in Primary_Mandatory_Card_Values) return HDU_Size_Info_Type;

        function  Get(MandVals : in Extension_Mandatory_Card_Values) return HDU_Size_Info_Type;

end Interpret;
