--
-- Notes:
--
-- FIXME make sure Ada Character type [Ada?][GNAT?]
-- is of same size as FITS Standard [FITS?] header-character


package body FITS.Header is


     -- [FITS 4.1.2 Components]:
     -- pos 9..10 is '= '
     -- pos 31 is comment ' /'
     -- then : pos 10..30 is value
   function To_Card(KeyName  : in String;
                    KeyValue : in String;
                    Comment  : in String) return Card_Type
   is
    Card : Card_Type := EmptyCard;
   begin

    Card(1  .. KeyName'Length)             := KeyName;
    Card(9  .. 10)                         := "= ";
    Card(11 .. (11 + KeyValue'Length - 1)) := KeyValue;
    Card(31 .. 32)                         := " /";
    Card(33 .. (33 + Comment'Length  - 1)) := Comment;
    -- FIXME will raise CONSTRAINT_ERROR if Length too big

    return Card;
   end To_Card;

end FITS.Header;
