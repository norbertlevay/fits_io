with FITS; use FITS;

-- Media (File, Network, etc)

-- implementation behind this interface operates on FITS.Card_Block
-- and so is agnostic of media where FITS file blocks are read from (disk File, network).
-- However Index and Set_Index restricts possible media and is needed only due to 2-scan parsing
-- (first parse HDU_Type then reset to header start and parse for HDU_Size)

generic
   type Source_Type is limited private;
   type Index_Type  is limited private;
   with function  Next(Source : in Source_Type) return FITS.Card_Block;
   with function  Index(Source : Source_Type) return Index_Type;
   with procedure Set_Index(Source : Source_Type; Index : Index_Type);
package FITS_IO.Media is

    -- Note on Read_*() below:
    -- before calling these funcs, set file-index to
    -- beging of the header.
    -- the functions read the gheader, so after return
    -- the file-index points to first position behind the header

   function Read_DUSize_bytes(Source : Source_Type)
     return FITS_IO.Count;


end FITS_IO.Media;
