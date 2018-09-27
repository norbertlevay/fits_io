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

   function To_Card(KeyName  : in String;
                    KeyValue : in String;
                    Comment  : in String) return Card_Type;

   --------------------------
   -- Parsing DU_Size_Type --
   --------------------------

   procedure Parse_Card_For_Size
              (Card          : in  Card_Type;
               DUSizeKeyVals : out DU_Size_Type);

   function  Write_Cards_For_Size
              (BITPIX : Integer;
               Dim    : AxesLengths_Arr ) return Card_Arr;

end FITS.Header;
