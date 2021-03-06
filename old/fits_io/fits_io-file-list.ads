
package FITS_IO.File.List is

   -----------------------
   -- FITS file content --
   -----------------------

   type HDU_Info(NAXIS : NAXIS_Type) is
     record
--        HDUType        : HDU_Type;
        CardsCnt       : FITS_IO.Count;        -- number of cards in this Header
        BITPIX         : Integer;              -- data type
        NAXISn         : NAXIS_Arr(1..NAXIS);  -- data dimensions
     end record;

   function Get (FitsFile : in  SIO.File_Type)
     return HDU_Info;
   -- File index must be set to start of the header before calling Get().

end FITS_IO.File.List;
