
package body FITS_IO.File.List is



   function Get (FitsFile : in  SIO.File_Type)
     return HDU_Info 
   is
    HDUInfo : HDU_Info(1);
   begin
    return HDUInfo;
   end Get;
   -- File index must be set to start of the header before calling Get().

end FITS_IO.File.List;
