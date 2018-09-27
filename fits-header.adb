--
-- Notes:
--
-- FIXME make sure Ada Character type [Ada?][GNAT?]
-- is of same size as FITS Standard [FITS?] header-character

with Ada.Strings.Fixed;
use  Ada.Strings.Fixed;


package body FITS.Header is

   -- calc number of free cards to fill up HeaderBlock
   function  Free_Card_Slots (CardsCnt : in FPositive ) return Natural
   is
    FreeSlotCnt : Natural := Natural( CardsCnt mod FPositive(CardsCntInBlock) );
    -- explicit conversion ok: mod < CardsCntInBlock = 36;
   begin
    if FreeSlotCnt /= 0 then
      FreeSlotCnt := CardsCntInBlock - FreeSlotCnt;
    end if;
    return FreeSlotCnt;
   end Free_Card_Slots;
   pragma Inline (Free_Card_Slots);


   -- BEGIN newIF : some dummy funcs
   function To_Card (Key     : in Max_8.Bounded_String;
                     Value   : in Max20.Bounded_String;
                     Comment : in Max48.Bounded_String)
                     return Card_Type
   is
    Card : Card_Type := EmptyCard;
   begin
    -- FIXME how to guarantee Key and Comment are right justified
    --       Value (often) left justified
    Card(1 .. 8) := Max_8.To_String(Key);
    Card(9 ..10) := "= ";
    Card(11..30) := Max20.To_String(Value);
    Card(31..32) := " /"; -- [FITS 4.1.2.3: "Space strongly recommended" ]
    Card(33..80) := Max48.To_String(Comment);
    return Card;
   end To_Card;

   function To_Card (Key     : in Max_8.Bounded_String;
                     Comment : in Max70.Bounded_String)
                     return Card_Type
   is
    Card : Card_Type := EmptyCard;
   begin
    -- FIXME implement!
    return Card;
   end To_Card;


     -- [FITS 4.1.2 Components]:
     -- pos 9..10 is '= '
     -- pos 31 is comment ' /'
     -- then : pos 10..30 is value
   function To_Card(KeyName  : in String;
                    KeyValue : in String;
                    Comment  : in String) return Card_Type
   is
    Card   : Card_Type := EmptyCard;
    ValStr : String(1 .. 20) := (others => ' ');
   begin
    -- [fitsverify complained: should be right aligned]
    ValStr(20 - (KeyValue'Length-1) .. 20 ) := KeyValue;

    Card(1  .. KeyName'Length) := KeyName;
    Card(9  .. 10)             := "= ";
    Card(11 .. 30)             := ValStr;-- right aligned text
    Card(31 .. 32)             := " /";
    Card(33 .. (33 + Comment'Length  - 1)) := Comment;
    -- FIXME will raise CONSTRAINT_ERROR if Length too big

    return Card;
   end To_Card;

   function  Write_Cards_For_Size
              (BITPIX : Integer;
               Dim    : NAXIS_Arr ) return Card_Arr
   is
    Cards : Card_Arr(1 .. (3 + Dim'Length));
   begin
    Cards(1) := To_Card("SIMPLE","T","Standard FITS file");
    Cards(2) := To_Card("BITPIX",Integer'Image(BITPIX)," ");
    -- verify BITPIX is legal value
    Cards(3) := To_Card("NAXIS", Positive'Image(Dim'Length)," ");

    for I in Dim'Range
    loop
      declare
       Idx : String := Positive'Image(I);
       Key : String := "NAXIS" & Idx(2 .. Idx'Last);
      begin
       Cards(3+I) := To_Card(Key,
                             FPositive'Image( Dim(I) )
                             ," ");
      end;
    end loop;

    return Cards;
   end Write_Cards_For_Size;


end FITS.Header;
