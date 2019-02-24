
-- Media (File, Network, etc)


generic
   type Source_Type is limited private;
   type Index_Type  is limited private;
   with function  Next(Source : in Source_Type) return Card_Block;
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
