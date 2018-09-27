--
-- Notes:
--
-- FIXME make sure Ada Character type [Ada?][GNAT?]
-- is of same size as FITS Standard [FITS?] header-character

with FITS.Size;  use FITS.Size;

package FITS.Header is

   ------------------
   -- Parse Header --
   ------------------

   -- FIXME replace with BoundedString variant (in FITS.FIle)
   function To_Card(KeyName  : in String;
                    KeyValue : in String;
                    Comment  : in String) return Card_Type;

   --------------------------
   -- Parsing DU_Size_Type --
   --------------------------

   -- FIXME IF-> this should be private:
   -- it is called in cycle for all Cards in Header
--   procedure Parse_Card_For_Size
--              (Card          : in  Card_Type;
--               DUSizeKeyVals : out DU_Size_Type);

   -- FIXME rename Carda_For_Size & and make it a function
   function  Write_Cards_For_Size
              (BITPIX : Integer;
               Dim    : NAXIS_Arr ) return Card_Arr;

end FITS.Header;
