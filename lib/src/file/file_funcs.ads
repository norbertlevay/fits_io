--
-- Various calculations formulas from [FITS].
--
-- FIXME error/exception handling missing



with FITS; use FITS; -- Positive_Count needed
--with FITS_IO; use FITS_IO; -- Positive_Count needed
--with Ada.Streams.Stream_IO;-- use Ada.Streams.Stream_IO; -- Positive_Count needed

with Mandatory; -- NAXIS_Array needed


package File_Funcs is


    function Data_Unit_Size_elems(NAXISn : NAXIS_Array)
        return Positive_Count;


    --
    -- Data array size calculations for all HDU types
    --

    -- implements [FITS] Eq(1)
    
    function PrimaryImage_DataSize_bits
        (BITPIX : Integer;
         NAXIS  : NAXIS_Array) return Positive_Count;


    -- implements [FITS] Eq(2)
    
    function ConformingExtension_DataSize_bits
        (BITPIX : Integer;
         NAXIS  : NAXIS_Array;
         PCOUNT : Count;
         GCOUNT : Positive_Count) return Positive_Count;


    -- implements [FITS] Eq(4) 

     function RandomGroups_DataSize_bits
        (BITPIX : Integer;
         NAXIS  : NAXIS_Array;
         PCOUNT : Count;
         GCOUNT : Positive_Count) return Positive_Count;


end File_Funcs;
