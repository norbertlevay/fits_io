

package body FITS.Parser is

   procedure Parse_Header(Source     : in Source_Type;
                          ParsedData : in out Parsed_Type;
                          UserData   : in out User_Type)
   is
    HBlk          : Card_Block;
    Card          : Card_Type;
    ENDCardFound  : Boolean := false;
    AllDataParsed : Boolean := false;
   begin
    loop
      -- [FITS] every valid FITS File must have at least one block
      HBlk := Next(Source);
      for I in HBlk'Range
      loop
        Card := HBlk(I);
        AllDataParsed := Parse_Card(Card, ParsedData, UserData); -- generic
        ENDCardFound  := (Card = ENDCard);
        exit when ENDCardFound OR AllDataParsed;
      end loop;
      exit when ENDCardFound OR AllDataParsed;
    end loop;
   end Parse_Header;

end FITS.Parser;
 