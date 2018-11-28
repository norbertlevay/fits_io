
with Ada.Streams.Stream_IO;

with Ada.Strings.Fixed; use  Ada.Strings.Fixed;
with Ada.Strings.Bounded; use  Ada.Strings.Bounded;

with FITS.Header; use FITS.Header;

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
   function Is_IndexedKey(ReadKey : in  Key_Type;
                          KeyRoot : in  Max_8.Bounded_String;
                          Index   : out Positive)
                   return Boolean
   is
     RKey      : String   := Trim(ReadKey,Ada.Strings.Both);
     RootLen   : Positive := Max_8.To_String(KeyRoot)'Length;
     RootMatch : Boolean  := RKey(1..RootLen) = Max_8.To_String(KeyRoot);
   begin
      if(RootMatch) then
       -- parse out the index value
       Index := Positive'Value(RKey(RootLen+1 .. RKey'Length));
       return True;
      else
       return False;
      end if;
   end Is_IndexedKey;



   procedure Parse(Card          : in Card_Type;
                   Keys_To_Parse : in Parse_Key_Arr;
                   Found_Keys    : in out Key_List.List)
   is
    Key : Parse_Key_Type;
    Idx : Positive;
    FoundKey : Key_Record_Type;
   begin
    for I in Keys_To_Parse'Range
    loop

       Key := Keys_To_Parse(I);

       if(Is_Key(Card(1..8),Key.Name))
       then
        -- pure Key match
        FoundKey.Name    := Max_8.To_Bounded_String(Trim(Card(1 .. 8),Ada.Strings.Both));
        FoundKey.Value   := Max20.To_Bounded_String(Trim(Card(10..30),Ada.Strings.Both));
        FoundKey.Comment := Max48.To_Bounded_String(Trim(Card(32..80),Ada.Strings.Both));
        Found_Keys.Append(FoundKey);
       elsif(Is_IndexedKey(Card(1..8),Key.Name,Idx))
       then
        -- Indexed key match maybe
        FoundKey.Name    := Max_8.To_Bounded_String(Trim(Card(1 .. 8),Ada.Strings.Both));
        FoundKey.Value   := Max20.To_Bounded_String(Trim(Card(10..30),Ada.Strings.Both));
        FoundKey.Comment := Max48.To_Bounded_String(Trim(Card(32..80),Ada.Strings.Both));
        -- here use also the returned index Idx
        Found_Keys.Append(FoundKey);
       end if;

       -- FIXME how to extend Key types: Key, IndexedKey, WCSKey,...?
       --
       -- use OOP instead if .. else if
       -- use Key'Class : Key.Match(NextCard,Key'Class) return Boolean
       -- if Match true -> Append Card to List
       -- E.g. Parsed_Key_Type should be tagged record

    end loop;
   end Parse;




   function Parse(Keys : in Parse_Key_Arr) return Key_Record_Arr
   is
    KeyRecs : Key_Record_Arr(1..2) ;
   begin
     -- Cards := Next; -- get next Card_Block from media (file, network, mem...)
     -- cycle throu all Cards in Card_Block,
     -- and compare them to Keys
     -- if match, fill in KeyRecs
     -- FIXME signal end if ENDCard found, How ?
     return KeyRecs;
   end Parse;


   function Parse_Mandatory return HDU_Size_Type
   is
    HDUSizeRec :  HDU_Size_Type ;
   begin
     return HDUSizeRec;
   end Parse_Mandatory;


end FITS.ParserA;
