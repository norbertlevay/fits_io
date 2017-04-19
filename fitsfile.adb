
with Ada.Text_IO; use Ada.Text_IO;
-- 3.1 Overall File Structure
-- FitsFile is an array of Header- or DataBlocks (HDU's).
-- PrimaryHDU;
-- ConformingExtensions; -- starts with "XTENSION" string
-- SpecialRecords; -- not start with "XTENSION" string, and can be whatever non-standard

with Ada.Characters.Handling;
use  Ada.Characters.Handling;

package body FitsFile is

 BlockSize : constant Natural := 2880; -- bytes

 -- 3.2 HeaderBlock only restricted set of (7bit ascii)
 -- chars: 32..126 (x20..x7E) -> " "(Space) .. "~"(Tilde)
 -- Ada.Standard.Ascii represents 7bit chars (considered outdated)

 type HeaderBlock is array(Positive range 1..CardsInBlockCnt) of Card;
 type Header is array(Positive range <>) of Card;

 type HeaderBlockBuffer is array(Positive range 1..CardsInBlockCnt)
                                                          of CardBuffer;

 PosInBlock_Default : constant Positive := CardsInBlockCnt + 1;
 PosInBlock  : Positive := PosInBlock_Default;
 BlockBuffer : String(1..BlockSize);

 procedure Initialize is
 begin
  PosInBlock := PosInBlock_Default;
 end Initialize;

 procedure Get(File : in File_Type; Item : out CardBuffer) is
  from : Positive;
 begin
      if PosInBlock > CardsInBlockCnt
      then
       Ada.Text_IO.Get(File, BlockBuffer);
       PosInBlock := 1;
      end if;

      from := (PosInBlock-1) * CardSize + 1;
      Item := BlockBuffer(from..(from + CardSize - 1));
      PosInBlock := PosInBlock + 1;
 end Get;


 -- Note: String is a Fixed.String in Ada
 function To_String ( Item : in Card )
 return String is
  mystr : String(1..CardSize) := (others => ' ');
 begin
      for i in Item'Range
      loop
      mystr(i) := Item(i);
      end loop;
  return mystr;
 end To_String;

 function To_Card ( Item : in String )
 return Card is
  mycard : Card := (others => ' ');
 begin

   if Item'Length <= CARDSIZE then
      for i in Item'Range 
      loop
      -- Note: if Item char is out of range of FitsChar:
      -- raised CONSTRAINT_ERROR : fitsfile.adb:NN range check failed
      mycard(i) := Item(i);
      end loop;
   end if;

   return mycard;
 end To_Card;

 function Is_Card ( Item : in String )
 return Boolean is
  c : Boolean := False;
  fc : FitsChar;
 begin

   if Item'Length <= CARDSIZE then
      for i in Item'Range
      loop
      -- Note: if Item char is out of range of FitsChar:
      -- raised CONSTRAINT_ERROR : fitsfile.adb:NN range check failed
      fc := Item(i);
      end loop;
      c := True;
   end if;

   return c;
 end Is_Card;

 function To_CardBuffer ( Item : in String )
 return CardBuffer is
  mycard : CardBuffer := (others => ' ');
 begin

   if Item'Length <= CardSize then
      for i in Item'Range 
      loop
      mycard(i) := Item(i);
      end loop;
   end if;

   return mycard;
 end To_CardBuffer;


 -- non-standard FitsFile, as sequence of 80-char strings
 function To_String ( Item : in CardBuffer;
  		      Substitute: in Character := '.' )
 return String is
  mystr : String(1..CardSize);
 begin
      for i in Item'Range
      loop
        if Is_Control(Item(i)) -- or Is_Special(Item(i))
        then
          mystr(i) := Substitute;
        else
          mystr(i) := Item(i);
        end if;

      end loop;
  return mystr;
 end To_String;

-- procedure Get(File : in File_Type; Item : out CardBuffer) is
-- begin
--      Ada.Text_IO.Get(File, Item);
-- end Get;

 -- Put "as is"
 procedure Put(File : in File_Type; Item : in CardBuffer) is
 begin
      Ada.Text_IO.Put(File, Item);
 end Put;

 -- When printing to terminal replace all non-printable charactes (with dot)
 procedure Put(Item : in CardBuffer) is
 begin
      Ada.Text_IO.Put(To_ISO_646(To_String(Item)));
 end Put;

end FitsFile;

