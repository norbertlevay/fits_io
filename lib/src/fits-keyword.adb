
--with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;


package body FITS.Keyword is


   function Match(Key  : in Keyword_Type;
                  Card : in Card_Type) return Boolean
   is
   begin
    return Max_8.To_String(Key.Name) = Trim( Card(1..8),Ada.Strings.Both);
   end Match;



   function Match(Key  : in Indexed_Keyword_Type;
                  Card : in Card_Type) return Boolean
   is
    Root    : constant String := Max_8.To_String(Key.Name);
    CardKey : constant String := Trim(Card(1..8),Ada.Strings.Both);
    ParsedIndex : Natural;
    Matched     : Boolean := False;
    HasIndex    : Boolean := Root'Length < CardKey'Length;
    RootMatched : Boolean := False;-- := Root = CardKey(1..Root'Length);
   begin

     if(Root'Length > CardKey'Length) then
       return False;
     end if;

     RootMatched := Root = CardKey(1..Root'Length);

    if (HasIndex AND RootMatched) then

     ParsedIndex := Natural'Value(CardKey(Root'Length+1 .. CardKey'Last));
     -- will raise exception if not a number

     if( (Key.Index_First <= ParsedIndex) AND
         (Key.Index_Last  >= ParsedIndex) ) then
        Matched := True;
     else
        -- FIXME out of range -> raise exception
        null;
     end if;

    end if;
    return Matched;
   end Match;


end FITS.Keyword;

