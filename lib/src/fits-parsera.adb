
with Ada.Text_IO;
with Ada.Tags; use Ada.Tags;

with Ada.Streams.Stream_IO;

with Ada.Strings.Fixed; use  Ada.Strings.Fixed;
with Ada.Strings.Bounded; use  Ada.Strings.Bounded;

with FITS.Header; use FITS.Header;


with Ada.Containers.Doubly_Linked_Lists;

package body FITS.ParserA is

   subtype Key_Type is String(1..8);

   -- all non-space characters must exactly match
   function Is_Key(ReadKey   : in Key_Type;
                   ParsedKey : in Max_8.Bounded_String)
                   return Boolean
   is
   begin
     -- trim spaces in prefixed and postfixed,
     -- remainig string must be exact match
     -- Note: standard requires keywords to start from 1st character
     -- in card (left justified). We ease this restriction for parsing
     -- allowing the keyword be anywhere within first 8 chars.
     -- This supports 'broken' headers to be fixed and still
     -- guarantees uniqueness/corectness.
     return (Trim(ReadKey,Ada.Strings.Both) = Max_8.To_String(ParsedKey));
   end Is_Key;


   -- parse keys of form KEYROOTnnn
--   function Is_IndexedKey(ReadKey : in  Key_Type;
--                          KeyRoot : in  Max_8.Bounded_String;
--                          Index   : out Positive)
--                   return Boolean
--   is
--     RKey      : String   := Trim(ReadKey,Ada.Strings.Both);
--     RootLen   : Positive := Max_8.To_String(KeyRoot)'Length;
--     RootMatch : Boolean  := RKey(1..RootLen) = Max_8.To_String(KeyRoot);
--   begin
--      if(RootMatch) then
       -- parse out the index value
--       Index := Positive'Value(RKey(RootLen+1 .. RKey'Length));
       -- FIXME return True only if:
       --    converts without error (handle exception here to avoid its propagation) and
       --    index is within range
--       return True;
--      else
--       return False;
--      end if;
--   end Is_IndexedKey;



   procedure Parse(Card          : in Card_Type;
                   Keys_To_Parse : in out In_Key_List.List;
                   Found_Keys    : in out Out_Key_List.List)
   is
    FoundKey : Key_Record_Type;
    Cursor   : In_Key_List.Cursor;
    Key      : Keyword_Ptr;
    OutLen  : Ada.Containers.Count_Type;
    InLen   : Ada.Containers.Count_Type;
   begin

--   Ada.Text_IO.Put_Line("NewCard: " & Card(1..8));

    Cursor := In_Key_List.First(Keys_To_Parse);
    while In_Key_List.Has_Element(Cursor)
    loop

       InLen  := In_Key_List.Length(Keys_To_Parse);
       OutLen := Out_Key_List.Length(Found_Keys);


        Key := In_Key_List.Element(Cursor);

--        Ada.Text_IO.Put_Line(Ada.Containers.Count_Type'Image(InLen) & " " & Ada.Containers.Count_Type'Image(OutLen));
--        Ada.Text_IO.Put_Line(Key'Tag);

        if(Match(Key.all,Card))
        then

         FoundKey.Name    := Max_8.To_Bounded_String(Trim(Card( 1.. 8),Ada.Strings.Both));
         FoundKey.Value   := Max20.To_Bounded_String(Trim(Card(10..30),Ada.Strings.Both));
         FoundKey.Comment := Max48.To_Bounded_String(Trim(Card(32..80),Ada.Strings.Both));

--         Ada.Text_IO.Put_Line(Max_8.To_String(FoundKey.Name));

         Found_Keys.Append(FoundKey);

         if (Key'Tag = Keyword_Type'Tag) then
          In_Key_List.Delete(Keys_To_Parse,Cursor);
         end if;

        end if;

       In_Key_List.Next(Cursor);

    end loop;

   end Parse;





   function Parse_Mandatory return HDU_Size_Type
   is
    HDUSizeRec :  HDU_Size_Type ;
   begin
     return HDUSizeRec;
   end Parse_Mandatory;




--   generic
--    type Source_Type is private;
--    with function Next(Source : in Source_Type) return Card_Block;
--   procedure Parse_Header(Source        : in Source_Type;
--                          Keys_To_Parse : in out In_Key_List.List;
--                          Found_Keys    : in out Out_Key_List.List)
   function Parse_Header(Source        : in Source_Type;
                         Keys_To_Parse : in In_Key_List.List)
     return Out_Key_List.List
   is
    Found_Keys : Out_Key_List.List;
    PKeys : In_Key_List.List := Keys_To_Parse;
    HBlk          : Card_Block;
    Card          : Card_Type;
    ENDCardFound  : Boolean := False;
    AllDataParsed : Boolean := False;-- FIXME file-index not pointing to DU
                                     -- if we leave before END card
                                     -- must continue reading until END card
                                     -- or
                                     -- reset file-index to begining of the Header
                                     -- Let user deal with this....?
   begin
    loop
      -- [FITS] every valid FITS File must have at least one block
      HBlk := Next(Source);
      for I in HBlk'Range
      loop
        Card := HBlk(I);
        AllDataParsed := False;--Parse_Card(Card, ParsedData, UserData); -- generic
--	Parse(Card,Keys_To_Parse,Found_Keys);
	Parse(Card,PKeys,Found_Keys);
        ENDCardFound  := (Card = ENDCard);
        exit when ENDCardFound OR AllDataParsed;
      end loop;
      exit when ENDCardFound OR AllDataParsed;
    end loop;
    return Found_Keys;
   end Parse_Header;



end FITS.ParserA;
