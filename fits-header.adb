--
-- Notes:
--
-- FIXME make sure Ada Character type [Ada?][GNAT?]
-- is of same size as FITS Standard [FITS?] header-character

with Ada.Strings.Fixed;
use  Ada.Strings.Fixed;


package body FITS.Header is


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

   -- Header Cards from which above data is taken

   -- parse from Card value if it is one of DU_Size_Type, do nothing otherwise
   -- and store parse value to DUSizeKeyVals
   -- TODO what to do if NAXIS and NAXISnn do not match in a broken FITS-file
   -- [FITS,Sect 4.4.1.1]: NAXISn keys _must_ match NAXIS keyword.
   -- Size calc is valid also for IMAGE-extension, but not for TABLE extensions
   -- FIXME should check if it is IMAGE extension [FITS, Sect 7]
   procedure Parse_Card (Card          : in Card_Type;
                         DUSizeKeyVals : out DU_Size_Type)
   is
    dim : Positive;
   begin
     -- FIXME what if parsed string is '' or '     ' etc...

     -- [FITS 4.1.2 Components]:
     -- pos 9..10 is '= '
     -- pos 31 is comment ' /'
     -- then : pos 10..20 is value

     if    (Card(1..9) = "BITPIX  =") then
       DUSizeKeyVals.BITPIX := Integer'Value(Card(10..30));

     elsif (Card(1..5) = "NAXIS") then

       if (Card(1..9) = "NAXIS   =") then
           DUSizeKeyVals.NAXIS := Positive'Value(Card(10..30));
       else
           dim := Positive'Value(Card(6..8));
           DUSizeKeyVals.NAXISn(dim) := FPositive'Value(Card(10..30));
           -- [FITS Sect 4.4.1.1] NAXISn is non-negative integer
           -- [FITS fixed integer]:
           -- Fixed integer is defined as 19 decimal digits
   	   -- (Header Card Integer value occupying columns 11..20)
   	   -- Lon_Long_Integer in GNAT is 64bit: 9.2 x 10**19 whereas
   	   -- fixed integer can reach 9.9 x 10**19)
           -- Conclude: range of NAXISn will be implementation
           -- limited as suggested in [FITS 4.2.3 Integer number]:
       end if;

     elsif (Card(1..5) = "PCOUNT") then
       DUSizeKeyVals.PCOUNT := FNatural'Value(Card(10..30));

     elsif (Card(1..5) = "GCOUNT") then
       DUSizeKeyVals.GCOUNT := FPositive'Value(Card(10..30));

     end if;

   end Parse_Card;

   -- not used FIXME
   procedure Parse_Card (Card         : in Card_Type;
                         XtensionType : out String)
   is
   begin
     if    (Card(1..9) = "XTENSION=") then
       XtensionType := Card(11..20);
     end if;
   end Parse_Card;

   procedure Parse_Card_For_Size
              (Card          : in  Card_Type;
               DUSizeKeyVals : out DU_Size_Type)
   is
   begin
    Parse_Card(Card, DUSizeKeyVals);
    DUSizeKeyVals.PCOUNT := 0;
    DUSizeKeyVals.GCOUNT := 1;
     -- init these for HDU's which do not use them
     -- BINTABLE and RandomGroup extensions, if present,
     -- will overwrite these values
   end Parse_Card_For_Size;

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
