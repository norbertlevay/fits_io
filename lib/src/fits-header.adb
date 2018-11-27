--
-- Notes:
--
-- FIXME make sure Ada Character type [Ada?][GNAT?]
-- is of same size as FITS Standard [FITS?] header-character

with Ada.Strings.Fixed; use  Ada.Strings.Fixed;
with Ada.Strings.Bounded; use  Ada.Strings.Bounded;


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

--    Card(1 .. 8) := Max_8.To_String(Key);
    Ada.Strings.Fixed.Move (Source  => Max_8.To_String(Key),
                        Target  => Card(1 .. 8),
                        Justify => Ada.Strings.Left, 
                        Drop    => Ada.Strings.Error, 
                        Pad     => ' '); 


    Card(9 ..10) := "= ";
    -- Card(11..30) := Max20.To_String(Value);
    Ada.Strings.Fixed.Move (Source  => Max20.To_String(Value),
                        Target  => Card(11 .. 30),
                        Justify => Ada.Strings.Right,
                        Drop    => Ada.Strings.Error, 
                        Pad     => ' '); 
    Card(31..32) := " /"; -- [FITS 4.1.2.3: "Space strongly recommended" ]
--    Card(33..80) := Max48.To_String(Comment);
    Ada.Strings.Fixed.Move (Source  => Max48.To_String(Comment),
                        Target  => Card(33 .. 80),
                        Justify => Ada.Strings.Left,
                        Drop    => Ada.Strings.Error, 
                        Pad     => ' '); 
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

   -------------
   -- Parsers --
   -------------

   procedure Parse_HDU_Type(Index: in  FPositive;
   			    Card : in  Card_Type;
                            Data : in out HDU_Type)
   is
   begin
     if    (Card(1..9) = "SIMPLE  =") then
       Data.SIMPLE    := Card(11..20);
     elsif (Card(1..9) = "XTENSION=") then
       Data.XTENSION  := Card(11..20);
     end if;
   end Parse_HDU_Type;

   -- DU_Size_Type collects keyword values which define DataUnit size
   -- parse from Card value if it is one of DU_Size_Type, do nothing otherwise
   -- and store parse value to DUSizeKeyVals
   -- TODO what to do if NAXIS and NAXISnn do not match in a broken FITS-file
   -- [FITS,Sect 4.4.1.1]: NAXISn keys _must_ match NAXIS keyword.
   -- Size calc is valid also for IMAGE-extension, but not for TABLE extensions
   -- FIXME should check if it is IMAGE extension [FITS, Sect 7]
   procedure Parse_HDU_Size_Type (Index   : in FPositive;
                                  Card    : in Card_Type;
                                  HDUSize : in out HDU_Size_Type)
   is
    dim : Positive;
   begin
    HDUSize.CardsCnt := Index;

     -- FIXME what if parsed string is '' or '     ' etc...

     -- [FITS 4.1.2 Components]:
     -- pos 9..10 is '= '
     -- pos 31 is comment ' /'
     -- then : pos 10..20 is value


     -- FIXME parse NAXISnnn to an array(1...999) allocated on heap
     -- when all parsing done generate NAXISn(1..NAXIS)
     -- Implement as: generic must have User_Area_Type private
     -- which is passed to Parse_Card(...,UA:User_Area_Type) and also
     -- (gen_)Read_Header(...,UA:User_Area_Type)
     -- and UserArea must be passed as param to Parse_Card

     if    (Card(1..9) = "BITPIX  =") then
       HDUSize.BITPIX := Integer'Value(Card(10..30));

     elsif (Card(1..5) = "NAXIS") then

       if (Card(1..9) = "NAXIS   =") then
           if 0 = Natural'Value(Card(10..30))
           then
             -- no data unit in this HDU
             -- FIXME [FITS 4.4.1.1 Primary Header] "A value of zero signifies
             -- that no data follow the header in the HDU."
             null;
           else
             HDUSize.NAXIS := Positive'Value(Card(10..30));
           end if;
       else
           dim := Positive'Value(Card(6..8));
           HDUSize.NAXISn(dim) := FPositive'Value(Card(10..30));
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
       HDUSize.PCOUNT := FNatural'Value(Card(10..30));

     elsif (Card(1..5) = "GCOUNT") then
       HDUSize.GCOUNT := FPositive'Value(Card(10..30));

     end if;

    -- FIXME this is no good
    HDUSize.PCOUNT := 0;
    HDUSize.GCOUNT := 1;
     -- init these for HDU's which do not use them
     -- BINTABLE and RandomGroup extensions, if present,
     -- will overwrite these values

   end Parse_HDU_Size_Type;

   ------------------------------
   -- KeyWord Parsing routines --
   ------------------------------

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

   type Key_Arr_FPos is record
    Index : Positive;
    Value : FPositive; -- whatever type as def'd by keystring
   end record;

   procedure Parse_HDU_Size_Type22
                    (Card      : in  Card_Type;
                     Data      : in out HDU_Size_Type;
                     UData     : in out HDU_Size_UserArea_Type)
   is
    -- key read from file
    Key : String(1..8) := Card(1..8);
    -- list keys to parse
    BITPIX : constant Max_8.Bounded_String := Max_8.To_Bounded_String("BITPIX");
    NAXIS  : constant Max_8.Bounded_String := Max_8.To_Bounded_String("NAXIS");
    NAXIS_Arr  : constant Max_8.Bounded_String := Max_8.To_Bounded_String("NAXIS");
    Index : Positive;
    temp : Key_Arr_FPos;
   begin
    -- UData is on Heap - can have the big arrays
    -- when END card parsed, set up arrays the correct arrays in HDU_Size_Type
    -- possible only if HDU_Size_Type returned from function
     if (Is_Key(Key, BITPIX)) then
       Data.BITPIX := Integer'Value(Card(10..30));

     elsif (Is_Key(Key, NAXIS)) then
       -- store arr length
       Data.NAXIS := Natural'Value(Card(10..30));

     elsif (Is_IndexedKey(Key, NAXIS_Arr, Index)) then
       -- collect all indexed key elements to NAXIS_List
       -- when parsing ready, convert list to array NAXIS_Arr(1..NAXIS)
       temp.Index := Index;
       temp.Value := FPositive'Value(Card(10..30));
       -- chain these temp's on a list
       -- when ENDCArd reached convert List to array, destroy list and return
     end if;

   end Parse_HDU_Size_Type22;

end FITS.Header;
