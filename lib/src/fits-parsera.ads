
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

   generic
    type Source_Type is private;
    with function Next(Source : in Source_Type) return Card_Block;
   procedure Parse_Header(Source        : in Source_Type;
                          Keys_To_Parse : in out In_Key_List.List;
                          Found_Keys    : in out Out_Key_List.List);

   -- BEGIN DU_Size child package (OR Mandatory child package ??)
   type NAXIS_Arr is array (Natural range <>) of Positive;

   type DU_Size_Type(NAXIS : Positive) is record
      BITPIX   : Integer;
      NAXISArr : NAXIS_Arr(1..NAXIS);
   end record;

   -- return number of axis
   function Naxis(ParsedKeys : in Out_Key_List.List)
     return Positive;

   function To_DU_Size_Type(ParsedKeys : in Out_Key_List.List)
     return DU_Size_Type;

   generic
    type Source_Type is private;
    with function Next(Source : in Source_Type) return Card_Block;
   function Parse_Header_For_DUSize(Source : in Source_Type)
     return DU_Size_Type;

   -- END DU_Size child package

   -- misc

   -- Consider below into FITS.ParserA.Mandatory
   function Parse_Mandatory return HDU_Size_Type;
   -- and similarly FITS.ParserA.ScaleData  FITS.ParserA.WCS etc...
   -- so extendibility solved by child-packages
   -- example: assume BINTABLE does not exist and is to be added
   -- so ParserA.Mandatory has only IMAGE and ASCIITABLE
   -- e.g. create FITS.ParserA.[?Mandatory.?]BINTABLE

end FITS.ParserA;
