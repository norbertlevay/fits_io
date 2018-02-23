--
-- Notes:
--
-- FIXME make sure Ada Character type [Ada?][GNAT?]
-- is of same size as FITS Standard [FITS?] header-character

with Ada.Strings.Fixed;
use  Ada.Strings.Fixed;


package body FITS.Header is


     -- [FITS 4.1.2 Components]:
     -- pos 9..10 is '= '
     -- pos 31 is comment ' /'
     -- then : pos 10..30 is value
   function To_Card(KeyName  : in String;
                    KeyValue : in String;
                    Comment  : in String) return Card_Type
   is
    Card   : Card_Type := EmptyCard;
    ValStr : String(1 .. 20) := (others => ' ');
   begin
    -- [fitsverify complained: should be right aligned]
    ValStr(20 - (KeyValue'Length-1) .. 20 ) := KeyValue;

    Card(1  .. KeyName'Length) := KeyName;
    Card(9  .. 10)             := "= ";
    Card(11 .. 30)             := ValStr;-- right aligned text
    Card(31 .. 32)             := " /";
    Card(33 .. (33 + Comment'Length  - 1)) := Comment;
    -- FIXME will raise CONSTRAINT_ERROR if Length too big

    return Card;
   end To_Card;

end FITS.Header;
