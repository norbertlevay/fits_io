
-- provides 'Read 'Write for Header- and Data Unit

-- covers two issues:
-- 1. Data alignment
-- 2. Endianness  -> moved to FITS, because solved on Scalar types, not DU arrays (Blocks)
--
-- Data structs reflect data-images inside FITS-files and so allow
-- (low level) Read / Write

with Interfaces;
with Ada.Streams.Stream_IO;


with FITS.Data;
use  FITS.Data;

package FITS.Block_IO is

   -- arrays

   type Float32Block_Arr is
     array ( 1 .. (2880*8) / Float_32'Size ) of Float_32;
   for Float32Block_Arr'Size use 2880*8;--BlockSize_bits;
   pragma Pack (Float32Block_Arr);

private

   procedure dummy;


end FITS.Block_IO;

