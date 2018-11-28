
with Ada.Strings.Bounded;
with Ada.Streams.Stream_IO;

with Ada.Containers.Doubly_Linked_Lists;


with FITS.Header; use FITS.Header;

package FITS.ParserA is

   package SIO renames Ada.Streams.Stream_IO;
   -- FIXME how to solve the "function Next return Card_Block" ?
   -- must not show SIO.File_Type  but is called
   -- inside Parse/Read_Header at each cycle until ENDCard found


   -- output: list of found Keys

   type Key_Record_Type is
    record
	Name    : Max_8.Bounded_String;
	Value   : Max20.Bounded_String;
	Comment : Max48.Bounded_String;
    end record;
    -- FIXME use variable record to cover
    -- ValuedKeyRecord like above and
    -- CommentKeyRecords: has no value

   package Key_List is
       new Ada.Containers.Doubly_Linked_Lists(Key_Record_Type);

   type Key_Record_Arr is array (Positive range <>) of Key_Record_Type;


   -- input: fixed table (constant array) of Keywords

   type Parse_Key_Type is
    record
	Name    : Max_8.Bounded_String;
        -- if indexed key, provide index range KEYmin .. KEYmax
        Min     : Natural;
        Max     : Positive;
    end record;
    -- FIXME use variable record for Key and IndexedKey
   type Parse_Key_Arr is array (Positive range <>) of Parse_Key_Type;

   -- do Parse

   procedure Parse(Card          : in Card_Type;
                   Keys_To_Parse : in Parse_Key_Arr;
                   Found_Keys    : in out Key_List.List);

   function Parse(Keys : in Parse_Key_Arr) return Key_Record_Arr;


   -- misc

   -- Consider below into FITS.ParserA.Mandatory
   function Parse_Mandatory return HDU_Size_Type;
   -- and similarly FITS.ParserA.ScaleData  FITS.ParserA.WCS etc...
   -- so extendibility solved by child-packages
   -- example: assume BINTABLE does not exist and is to be added
   -- so ParserA.Mandatory has only IMAGE and ASCIITABLE
   -- e.g. create FITS.ParserA.[?Mandatory.?]BINTABLE

end FITS.ParserA;
