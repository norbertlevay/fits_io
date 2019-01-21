
with Ada.Streams.Stream_IO;
with FITS.Header; use FITS.Header;-- Max20.

package FITS_IO.File is

   package SIO renames Ada.Streams.Stream_IO;

   -----------------------
   -- FITS file content --
   -----------------------

   type HDU_Info_Type(NAXIS : Positive) is
   record
      XTENSION : Max20.Bounded_String; -- XTENSION value or empty
      CardsCnt : FPositive;            -- number of cards in this Header
      BITPIX   : Integer;              -- data type
      NAXISn   : NAXIS_Arr(1..NAXIS);  -- data dimensions
   end record;

   function  Get (FitsFile : in  SIO.File_Type)
      return HDU_Info_Type;

   -------------------------
   -- Positioning in file --
   -------------------------

   procedure Set_Index(FitsFile : in SIO.File_Type;
                       HDUNum   : in Positive);

end FITS_IO.File;
