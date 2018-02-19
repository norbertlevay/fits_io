--
-- Implementation notes:
--
-- FIXME make sure Ada Character type [Ada?][GNAT?]
-- is of same size as FITS Standard [FITS?] header-character

with Ada.Unchecked_Conversion;

with Interfaces;
use  Interfaces;

with System;
use  System;

with FITS.Header;
use  FITS.Header;

package body FITS is

   procedure dummy is
   begin
    null;
   end dummy;



end FITS;

