
with Ada.Strings.Bounded;

with Ada.Containers.Doubly_Linked_Lists;


with FITS.Header; use FITS.Header;
with FITS.Keyword; use FITS.Keyword;

package FITS.ParserA is

   -- input: list of constant/literal Keywords of different type

   type Keyword_Ptr is access all Keyword_Type'Class;

   package In_Key_List is
       new Ada.Containers.Doubly_Linked_Lists(Keyword_Ptr);
   use In_Key_List;


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

   package Out_Key_List is
       new Ada.Containers.Doubly_Linked_Lists(Key_Record_Type);
   use Out_Key_List;

   type Key_Record_Arr is array (Positive range <>) of Key_Record_Type;



   -- Parse one card

   procedure Parse(Card          : in Card_Type;
                   Keys_To_Parse : in out In_Key_List.List;
                   Found_Keys    : in out Out_Key_List.List);

   -- Parse all needed cards

   generic
    type Source_Type is private;
    with function Next(Source : in Source_Type) return Card_Block;
   function Parse_Header(Source        : in Source_Type;
                         Keys_To_Parse : in out In_Key_List.List)
     return Out_Key_List.List;


   -- misc

   -- Consider below into FITS.ParserA.Mandatory
   function Parse_Mandatory return HDU_Size_Type;
   -- and similarly FITS.ParserA.ScaleData  FITS.ParserA.WCS etc...
   -- so extendibility solved by child-packages
   -- example: assume BINTABLE does not exist and is to be added
   -- so ParserA.Mandatory has only IMAGE and ASCIITABLE
   -- e.g. create FITS.ParserA.[?Mandatory.?]BINTABLE

end FITS.ParserA;
