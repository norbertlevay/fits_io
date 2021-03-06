
-- see [Barnes p516] Enumeration_IO: prints 'X' includes single quotes

-- FIXME write   FITS-IO.File.List.ads
-- list all HDUs in FITS file:
-- List(FistFile) return array of HDU_Info

with Ada.Streams.Stream_IO;

package FITS_IO.File is

   package SIO renames Ada.Streams.Stream_IO;

   -------------------------
   -- Positioning in file --
   -------------------------

   procedure Set_Index(FitsFile : in SIO.File_Type;
                       HDUNum   : in Positive);

end FITS_IO.File;
