


with Mandatory; -- NAXIS_Array needed

package body File_Funcs is


    function Data_Unit_Size_elems(NAXISn : NAXIS_Array) return Positive_Count
    is
        Elem_Count : Positive_Count := 1;
    begin

        for I in NAXISn'Range
        loop
            Elem_Count := Elem_Count * NAXISn(I);
        end loop;

        return Elem_Count;
    end Data_Unit_Size_elems;




    -- implements [FITS] Eq(1)

        function PrimaryImage_DataSize_bits
                (BITPIX : Integer;
                 NAXIS  : NAXIS_Array) return Positive_Count
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
                 NAXIS  : NAXIS_Array;
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
                 NAXIS  : NAXIS_Array;
                 PCOUNT : Count;
                 GCOUNT : Positive_Count) return Positive_Count
    is
        NAXISCopy : NAXIS_Array := NAXIS;
    begin
        
        -- in RandomGroups NAXIS1 = 0
        -- with NAXIS=1, the formula (4) is the same as (2)
        
        NAXISCopy(1) := 1;

        return ConformingExtension_DataSize_bits(BITPIX,NAXISCopy,PCOUNT,GCOUNT);

        end RandomGroups_DataSize_bits;
    
    



end File_Funcs;
