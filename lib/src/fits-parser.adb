
with FITS.File; use FITS.File;

package body FITS.Parser is

  -- Card access

  function Next return Card_Block
  is
  begin
   return Read_Cards (File);
   -- FIXME Next should be generic
   -- if instantiated for File, this would be its implementation
   -- if instantiated for memory area, implemntaion would be positioning in memory to next block
  end Next;


 -- Parsing

  procedure Parse_Card(Card : in Card_Type;
                       RK   : in out Root_Keys)
  is
  begin
   RK.ENDCard := ENDCard = Card;
  end Parse_Card;

  procedure Parse_Card(Card : in Card_Type;
                       Ks   : in out Keys)
  is
  begin
   null;-- put parsed Card value to KR
  end Parse_Card;

  procedure Parse_Card(Card : in Card_Type;
                       SK   : in out Primary_Keys)
  is
  begin
   Parse_Card(Card, Keys(SK));
   -- parse here the new keys
  end Parse_Card;

  function DU_Size_blocks (KR : in Primary_Keys)
    return Natural
  is
   Size : Natural := 0;
  begin
   -- FIXME do calc ...
   return Size;
  end DU_Size_blocks;


  procedure Parse (KRC : in out Root_Keys'Class)
  is
    HBlk         : Card_Block;
    Card         : Card_Type;
    ENDCardFound : Boolean := false;
  begin
    loop
      HBlk := Next; -- next HeaderBlock
      -- [FITS] every valid FITS File must have at least one block
      for I in HBlk'Range
      loop
        Card := HBlk(I);
        Parse_Card(Card, KRC);
        ENDCardFound := KRC.ENDCard;--(Card = ENDCard);
        exit when ENDCardFound;
      end loop;
      exit when ENDCardFound;
    end loop;
  end Parse;

end FITS.Parser;

