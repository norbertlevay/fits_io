
--with Ada.Text_IO;
with Ada.Tags; use Ada.Tags;
with Ada.Strings.Fixed;   use  Ada.Strings.Fixed;

with FITS.Header; use FITS.Header;-- Max_8


with Ada.Containers.Doubly_Linked_Lists;

package body FITS.Parser is


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

    Cursor := In_Key_List.First(Keys_To_Parse);
    while In_Key_List.Has_Element(Cursor)
    loop

       InLen  := In_Key_List.Length(Keys_To_Parse);
       OutLen := Out_Key_List.Length(Found_Keys);

        Key := In_Key_List.Element(Cursor);

        if(Match(Key.all,Card))
        then

         FoundKey.Name    := Max_8.To_Bounded_String(Trim(Card( 1.. 8),Ada.Strings.Both));
         FoundKey.Value   := Max20.To_Bounded_String(Trim(Card(10..30),Ada.Strings.Both));
         FoundKey.Comment := Max48.To_Bounded_String(Trim(Card(32..80),Ada.Strings.Both));

         Found_Keys.Append(FoundKey);

         if (Key'Tag = Keyword_Type'Tag) then
          In_Key_List.Delete(Keys_To_Parse,Cursor);
         end if;

        end if;

       In_Key_List.Next(Cursor);

    end loop;

   end Parse;



   function Parse_Header(Source        : in Source_Type;
                         Keys_To_Parse : in In_Key_List.List)
     return Out_Key_List.List
   is
    Found_Keys : Out_Key_List.List;
    PKeys : In_Key_List.List := Keys_To_Parse;
    HBlk          : Card_Block;
    Card          : Card_Type;
    ENDCardFound  : Boolean := False;
    AllDataParsed : Boolean := False;
     -- FIXME file-index not pointing to DU
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
        AllDataParsed := False;
	Parse(Card,PKeys,Found_Keys);
        ENDCardFound  := (Card = ENDCard);
        exit when ENDCardFound OR AllDataParsed;
      end loop;
      exit when ENDCardFound OR AllDataParsed;
    end loop;
    return Found_Keys;
   end Parse_Header;



end FITS.Parser;
