
with Ada.Containers.Doubly_Linked_Lists;

with FITS.Keyword; use FITS.Keyword;

generic
   type Source_Type is limited private;
   with function Next(Source : in Source_Type) return Card_Block;
package FITS.Parser is

   -- input: list of literal Keywords of different type

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


   -- Parse all requested cards

   function Parse_Header(Source        : in Source_Type;
                         Keys_To_Parse : in out In_Key_List.List;
                         Found_Keys    : in out Out_Key_List.List)
     return Positive;

end FITS.Parser;
