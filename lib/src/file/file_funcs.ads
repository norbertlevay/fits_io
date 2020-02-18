--
-- Various calculations formulas from [FITS].
--
-- FIXME error/exception handling missing

--with Keyword_Record; use  Keyword_Record;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO; -- Positive_Count needed

with Mandatory; -- NAXIS_Arr needed


package File_Funcs is
        
    --
    -- Data array size calculations for all HDU types
    --

    -- implements [FITS] Eq(1)
    
    function PrimaryImage_DataSize_bits
        (BITPIX : Integer;
         NAXIS  : Mandatory.NAXIS_Arr) return Positive_Count;


    -- implements [FITS] Eq(2)
    
    function ConformingExtension_DataSize_bits
        (BITPIX : Integer;
         NAXIS  : Mandatory.NAXIS_Arr;
         PCOUNT : Count;
         GCOUNT : Positive_Count) return Positive_Count;


    -- implements [FITS] Eq(4) 

     function RandomGroups_DataSize_bits
        (BITPIX : Integer;
         NAXIS  : Mandatory.NAXIS_Arr;
         PCOUNT : Count;
         GCOUNT : Positive_Count) return Positive_Count;


end File_Funcs;
