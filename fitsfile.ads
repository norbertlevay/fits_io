
with Ada.Text_IO; use Ada.Text_IO;

package FitsFile is

 CardsInBlockCnt : constant Natural := 36; -- = BlockSize/CardSize
 CardSize        : constant Natural := 80; -- bytes
 subtype FitsChar is Character range Character'Val(32) .. Character'Val(126);
 type Card is array(Positive range 1..CardSize) of FitsChar;

 procedure Initialize;

 -- conversions

 function Is_Card ( Item : in String ) return Boolean;

 function To_Card ( Item : in String ) return Card;
 function To_String ( Item : in Card ) return String;

 -- non-standard, low level access
 -- FitsFile viewed as sequence Cards (80-Char/8bit Strings)

 subtype CardBuffer is String(1..CardSize);

 function To_CardBuffer ( Item : in String ) return CardBuffer;

 procedure Get(File : in File_Type; Item : out CardBuffer);

 procedure Put(File : in File_Type; Item : in  CardBuffer);
 procedure Put(Item : in CardBuffer);


end FitsFile;

