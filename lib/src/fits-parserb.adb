--
-- Notes:
--
-- FIXME make sure Ada Character type [Ada?][GNAT?]
-- is of same size as FITS Standard [FITS?] header-character

with Ada.Strings.Fixed; use  Ada.Strings.Fixed;

with FITS.File; use FITS.File;

package body FITS.ParserB is

   function  gen_Read_Header (FitsFile : in SIO.File_Type)
     return  Parsed_Type
   is
    HBlk         : Card_Block;
    Card         : Card_Type;
    ENDCardFound : Boolean := false;
    CardIdx      : FNatural := 0;
    Data         : Parsed_Type;

--  FIXME consider Data be on Heap because some Parsed_Type
--  can be unconvetionally big for stack (like Size_Type with NAXIS999).
--  However if Data on heap: caller must free it !! e.g. :
--  Free_Data must be done by caller
--
--    type Data_Acc is access all Parsed_Type;
--    procedure Free_Data is
--          new Ada.Unchecked_Deallocation(Parsed_Type, Data_Acc);
--    Data_Ptr : Data_Acc := new Parsed_Type;
--  in code then dereference: use Data_Ptr.all where is Data now

   begin
    loop
      HBlk := Read_Cards(FitsFile);
      -- [FITS] every valid FITS File must have at least one block
      for I in HBlk'Range
      loop
        Card         := HBlk(I);
        CardIdx      := CardIdx + 1;
        Parse_Card(CardIdx,Card, Data); -- generic
        ENDCardFound := (Card = ENDCard);
        exit when ENDCardFound;
      end loop;
      exit when ENDCardFound;
    end loop;
    return Data;
   end gen_Read_Header;

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

   procedure Parse_HDU_Size_Type22
                    (Card      : in  Card_Type;
                     Data      : in out HDU_Size_Type;
                     UData     : in out HDU_Size_UserArea_Type)
   is
    dim : Positive;
   begin
    -- UData is on Heap - can have the big arrays
    -- when END card parsed, set up arrays the correct arrays in HDU_Size_Type
    -- possible only if HDU_Size_Type returned from function
    null;
   end Parse_HDU_Size_Type22;

end FITS.ParserB;
