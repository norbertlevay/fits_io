
with Ada.Strings.Bounded;
with Ada.Streams.Stream_IO;

with FITS.Header; use FITS.Header;

package FITS.ParserA is

   package SIO renames Ada.Streams.Stream_IO;

   type Key_Record_Type is
    record
	Key     : Max_8.Bounded_String;
	Value   : Max20.Bounded_String;
	Comment : Max48.Bounded_String;
    end record;
    -- FIXME use variable record to cover
    -- ValuedKeyRecord like above and
    -- CommentKeyRecords: has no value
   type Key_Record_Arr is array (Positive range <>) of Key_Record_Type;

   type Parse_Key_Type is
    record
	Key     : Max_8.Bounded_String;
        -- if indexed key, provide index range KEYmin .. KEYmax
        Min     : Natural;
        Max     : Positive;
    end record;
    -- FIXME use variable record for Key and IndexedKey
   type Parse_Key_Arr is array (Positive range <>) of Parse_Key_Type;

   function Parse(Keys : in Parse_Key_Arr) return Key_Record_Arr;

end FITS.ParserA;
