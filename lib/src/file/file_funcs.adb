


with Mandatory; -- NAXIS_Arr needed
with Keyword_Record;
use Keyword_Record;

package body File_Funcs is

    -- implements [FITS] Eq(1)

        function PrimaryImage_DataSize_bits
                (BITPIX : Integer;
                 NAXIS  : Mandatory.NAXIS_Arr) return Positive_Count
    is
        Nbits : Positive_Count := 1;
    begin

        for I in NAXIS'Range
        loop
            Nbits := Nbits * NAXIS(I);
        end loop;

        Nbits := Positive_Count(abs BITPIX) * Nbits;
        
        return Nbits;

    end PrimaryImage_DataSize_bits;


        -- implements [FITS] Eq(2)
 
        function ConformingExtension_DataSize_bits
                (BITPIX : Integer;
                 NAXIS  : Mandatory.NAXIS_Arr;
                 PCOUNT : Count;
                 GCOUNT : Positive_Count) return Positive_Count
    is
        Nbits : Positive_Count := PrimaryImage_DataSize_bits(BITPIX,NAXIS);
    begin

        Nbits := Nbits + Positive_Count(abs BITPIX) * PCOUNT;
        Nbits := Nbits * GCOUNT;

        return Nbits;

    end ConformingExtension_DataSize_bits;
    
    
        -- implements [FITS] Eq(4) 
    
    function RandomGroups_DataSize_bits
                (BITPIX : Integer;
                 NAXIS  : Mandatory.NAXIS_Arr;
                 PCOUNT : Count;
                 GCOUNT : Positive_Count) return Positive_Count
    is
        NAXISCopy : Mandatory.NAXIS_Arr := NAXIS;
    begin
        
        -- in RandomGroups NAXIS1 = 0
        -- with NAXIS=1, the formula (4) is the same as (2)
        
        NAXISCopy(1) := 1;

        return ConformingExtension_DataSize_bits(BITPIX,NAXISCopy,PCOUNT,GCOUNT);

        end RandomGroups_DataSize_bits;
    
    



end File_Funcs;
