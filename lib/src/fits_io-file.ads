
-- see [Barnes p516] Enumeration_IO: prints 'X' includes single quotes

-- FIXME write   FITS-IO.File.List.ads
-- list all HDUs in FITS file:
-- List(FistFile) return array of HDU_Info

with Ada.Streams.Stream_IO;

package FITS_IO.File is

   package SIO renames Ada.Streams.Stream_IO;

   -----------------------
   -- FITS file content --
   -----------------------

   type HDU_Type is
       (PrimaryHeader,
        RandomGroups,
        Image,
        AsciiTable,
        BinaryTable);

   subtype Primary   is HDU_Type range PrimaryHeader .. RandomGroups;
   subtype Extension is HDU_Type range Image         .. BinaryTable;

   type HDU_Info(NAXIS : NAXIS_Type) is
     record
        HDUType        : HDU_Type;
        CardsCnt       : FITS_IO.Count;        -- number of cards in this Header
        BITPIX         : Integer;              -- data type
        NAXISn         : NAXIS_Arr(1..NAXIS);  -- data dimensions
     end record;

   function Get (FitsFile : in  SIO.File_Type)
     return HDU_Info;
   -- File index must be set to start of the header before calling Get().

   -------------------------
   -- Positioning in file --
   -------------------------

   procedure Set_Index(FitsFile : in SIO.File_Type;
                       HDUNum   : in Positive);

end FITS_IO.File;
