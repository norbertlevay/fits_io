
-- should contain all Header related definitions and functions
-- like Keywords/Cards, parsers for structures (=groups of
-- related keywords), and structure-related
-- conputations (DUSize, WCS coords, ...)

-- FIXME use the same generic as Parser - to _separate_
-- this code from FITS_IO.File (no need for Ada.Stream_IO here)

with Ada.Strings.Bounded; use Ada.Strings.Bounded;


generic
   type Source_Type is limited private;
   type Index_Type  is limited private;
   with function Next(Source : in Source_Type) return Card_Block;
   with function Index(Source : Source_Type) return Index_Type;
   with procedure Set_Index(Source : Source_Type; Index : Index_Type);
package FITS_IO.Header is

   type HDU_Type is
       (PrimaryHeader,
        RandomGroups,
        Image,
        AsciiTable,
        BinaryTable);

   subtype Primary   is HDU_Type range PrimaryHeader .. RandomGroups;
   subtype Extension is HDU_Type range Image         .. BinaryTable;


    -- Note on Read_*() below:
    -- before calling these funcs, set file-index to
    -- beging of the header.
    -- the functions read the gheader, so after return
    -- the file-index points to first position behind the header

   function Read_DUSize_bytes(Source : Source_Type)
     return FITS_IO.Count;


end FITS_IO.Header;
