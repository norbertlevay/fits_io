--
-- Notes:
--
-- FIXME make sure Ada Character type [Ada?][GNAT?]
-- is of same size as FITS Standard [FITS?] header-character

with Ada.Strings.Bounded;


package FITS.Header is

   --
   -- calc number of free cards to fill up HeaderBlock
   --
   function  Free_Card_Slots (CardsCnt : in FPositive ) return Natural;
   --  always 0..35 < 36(=Cards per Block)

   ------------------
   -- Parse Header --
   ------------------

   -- FIXME replace with BoundedString variant (in FITS.FIle)
   function To_Card(KeyName  : in String;
                    KeyValue : in String;
                    Comment  : in String) return Card_Type;

   package Max_8 is
       new Ada.Strings.Bounded.Generic_Bounded_Length (Max =>  8);
   package Max20 is
       new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 20);
   package Max48 is
       new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 48);
   package Max70 is
       new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 70);

   function To_Card (Key     : in Max_8.Bounded_String;
                     Value   : in Max20.Bounded_String;
                     Comment : in Max48.Bounded_String)
                     return Card_Type;
    -- for cards with value

   function To_Card (Key     : in Max_8.Bounded_String;
                     Comment : in Max70.Bounded_String)
                     return Card_Type;
    -- for cards with text (like HISTORY or COMMENT, see FITS 4.1.2.2)

    -- FIXME add one more: for cards with Key and long Value (see FITS 4.1.2.3)

   --------------------------
   -- Parsing DU_Size_Type --
   --------------------------

   -- FIXME rename to Cards_For_Size & and make it a function
   function  Write_Cards_For_Size
              (BITPIX : Integer;
               Dim    : NAXIS_Arr ) return Card_Arr;

end FITS.Header;
