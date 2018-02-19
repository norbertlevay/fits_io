
-- provides 'Read 'Write for Header- and Data Unit

-- covers two issues:
-- 1. Data alignment
-- 2. Endianness
--
-- Data structs reflect data-images inside FITS-files and so allow
-- (low level) Read / Write

with Interfaces;
with Ada.Streams.Stream_IO;


package FITS.Block_IO is

   -- arrays

   type Float32Block_Arr is
     array ( 1 .. BlockSize_bits / Float_32'Size ) of Float_32;
   for Float32Block_Arr'Size use BlockSize_bits;
   pragma Pack (Float32Block_Arr);

private

   procedure dummy;


end FITS.Block_IO;

