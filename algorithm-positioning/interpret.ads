


with FITS; use FITS;

with FA_Primary;   use FA_Primary;
with FA_Extension; use FA_Extension;


package Interpret 
is
        function  Get(MandVals : in Primary_Mandatory_Card_Values) return HDU_Size_Info_Type;

        function  Get(MandVals : in Extension_Mandatory_Card_Values) return HDU_Size_Info_Type;

end Interpret;
