
--with Ada.Text_IO;
with Ada.Tags; use Ada.Tags;
with Ada.Strings.Fixed;   use  Ada.Strings.Fixed;

with FITS.Header; use FITS.Header;-- Max_8


with Ada.Containers.Doubly_Linked_Lists;

package body FITS.Parser is


   procedure Parse(Card          : in Card_Type;
                   Keys_To_Parse : in out In_Key_List.List;
                   Found_Keys    : in out Out_Key_List.List)
   is
    FoundKey : Key_Record_Type;
    Cursor   : In_Key_List.Cursor;
    Key      : Keyword_Ptr;
    OutLen  : Ada.Containers.Count_Type;
    InLen   : Ada.Containers.Count_Type;
   begin

    Cursor := In_Key_List.First(Keys_To_Parse);
    while In_Key_List.Has_Element(Cursor)
    loop

       InLen  := In_Key_List.Length(Keys_To_Parse);
       OutLen := Out_Key_List.Length(Found_Keys);

        Key := In_Key_List.Element(Cursor);

        if(Match(Key.all,Card))
        then

         FoundKey.Name    := Max_8.To_Bounded_String(Trim(Card( 1.. 8),Ada.Strings.Both));
         FoundKey.Value   := Max20.To_Bounded_String(Trim(Card(10..30),Ada.Strings.Both));
         FoundKey.Comment := Max48.To_Bounded_String(Trim(Card(32..80),Ada.Strings.Both));

         Found_Keys.Append(FoundKey);

         if (Key'Tag = Keyword_Type'Tag) then
          In_Key_List.Delete(Keys_To_Parse,Cursor);
         end if;

        end if;

       In_Key_List.Next(Cursor);

    end loop;

   end Parse;



   function Parse_Header(Source        : in Source_Type;
                         Keys_To_Parse : in out In_Key_List.List;
                         Found_Keys    : in out Out_Key_List.List)
     return Positive
   is
    PKeys : In_Key_List.List := Keys_To_Parse;
    HBlk          : Card_Block;
    Card          : Card_Type;
    CardsCnt      : Natural := 0;
    ENDCardFound  : Boolean := False;
    AllDataParsed : Boolean := False;
     -- FIXME file-index not pointing to DU
     -- if we leave before END card
     -- must continue reading until END card
     -- or
     -- reset file-index to begining of the Header
     -- Let user deal with this....?
   begin
    loop
      -- [FITS] every valid FITS File must have at least one block
      HBlk := Next(Source);
      for I in HBlk'Range
      loop
        Card := HBlk(I);
        CardsCnt := CardsCnt + 1;
        AllDataParsed := False; -- FIXME not in use: affects CardsCnt and FileIndex
	Parse(Card,PKeys,Found_Keys);
        ENDCardFound  := (Card = ENDCard);
        exit when ENDCardFound OR AllDataParsed;
      end loop;
      exit when ENDCardFound OR AllDataParsed;
    end loop;
    return CardsCnt;
   end Parse_Header;


   -- NEW : recognize HDU type: Primary, IMAGE-ext, ASCIITABLE-ext...
   -- and none of previous (e.g. experimental HDU)

   procedure Init_List_Primary(InKeys : in out In_Key_List.List)
   is
    bitpix_ptr   : Keyword_Ptr := new Keyword_Type'(Name => Max_8.To_Bounded_String("BITPIX"));
    naxis_ptr    : Keyword_Ptr := new Keyword_Type'(Name => Max_8.To_Bounded_String("NAXIS"));
    naxisarr_ptr : Keyword_Ptr := new Indexed_Keyword_Type'(Name => Max_8.To_Bounded_String("NAXIS"),
                                                           Index_First =>  1,
                                                           Index_Last  =>999,
                                                           Index       =>0);
   begin
    InKeys.Append(bitpix_ptr);
    InKeys.Append(naxis_ptr);
    InKeys.Append(naxisarr_ptr);
   end Init_List_Primary;

   procedure Init_List_Add_RandomGroups(InKeys : in out In_Key_List.List)
   is
    groups_ptr   : Keyword_Ptr := new Keyword_Type'(Name => Max_8.To_Bounded_String("GROUPS"));
   begin
    InKeys.Append(groups_ptr);
   end Init_List_Add_RandomGroups;


   procedure Init_List_Add_Extension(InKeys : in out In_Key_List.List)
   is
    pcount_ptr   : Keyword_Ptr := new Keyword_Type'(Name => Max_8.To_Bounded_String("PCOUNT"));
    gcount_ptr   : Keyword_Ptr := new Keyword_Type'(Name => Max_8.To_Bounded_String("GCOUNT"));
   begin
    InKeys.Append(pcount_ptr);
    InKeys.Append(gcount_ptr);
   end Init_List_Add_Extension;


   procedure Init_List_Add_BinTable(InKeys : in out In_Key_List.List)
   is
    tfields_ptr  : Keyword_Ptr := new Keyword_Type'(Name => Max_8.To_Bounded_String("TFIELDS"));
    tformarr_ptr : Keyword_Ptr := new Indexed_Keyword_Type'(Name => Max_8.To_Bounded_String("TFORM"),
                                                           Index_First =>  1,
                                                           Index_Last  =>999,
                                                           Index       =>0);
   begin
    InKeys.Append(tfields_ptr);
    InKeys.Append(tformarr_ptr);
   end Init_List_Add_BinTable;

   procedure Init_List_Add_AsciiTable(InKeys : in out In_Key_List.List)
   is
    tbcolarr_ptr : Keyword_Ptr := new Indexed_Keyword_Type'(Name => Max_8.To_Bounded_String("TBCOL"),
                                                           Index_First =>  1,
                                                           Index_Last  =>999,
                                                           Index       =>0);
   begin
    InKeys.Append(tbcolarr_ptr);
   end Init_List_Add_AsciiTable;

   procedure InitList(Card          : in Card_Type;
                      Keys_To_Parse : in out In_Key_List.List)
   is
   begin
      if (Card(1..5) = "SIMPLE") then
         --  primary (or RandomGroup)

         if(Card(11..11) = "T") then
           -- Primary Header, conforming with standard
           Init_List_Primary(Keys_To_Parse);

         elsif(Card(11..11) = "F") then
           -- Primary Header, might not be conforming with standard
           -- FIXME WHat to do ?
           null;

         else
           -- raise exception
           null;

         end if;

         -- RandomGroup ??

      elsif (Card(1..8) = "XTENSION") then
         Init_List_Primary(Keys_To_Parse);
         Init_List_Add_Extension(Keys_To_Parse);

         if(Card(11..15) = "IMAGE") then
           null;

         elsif(Card(11..20) = "ASCIITABLE") then
           Init_List_Add_BinTable(Keys_To_Parse);
           Init_List_Add_AsciiTable(Keys_To_Parse);

         elsif(Card(11..18) = "BINTABLE") then
           Init_List_Add_BinTable(Keys_To_Parse);

         else
           -- undefed conforming extension
           null;
         end if;

      else
         -- experimantal
         null;
      end if;


   end InitList;

   -- working copy: recognize Header type and setup accordingly the InList
   function wParse_Header(Source        : in Source_Type;
                         Keys_To_Parse : in out In_Key_List.List;
                         Found_Keys    : in out Out_Key_List.List)
     return Positive
   is
    HBlk          : Card_Block;
    Card          : Card_Type;
    CardsCnt      : Natural := 0;
    ENDCardFound  : Boolean := False;
    AllDataParsed : Boolean := False;
     -- FIXME file-index not pointing to DU
     -- if we leave before END card
     -- must continue reading until END card
     -- or
     -- reset file-index to begining of the Header
     -- Let user deal with this....?
   begin
    loop
      -- [FITS] every valid FITS File must have at least one block


      -- get 1st block

      -- do for 1st card
      HBlk := Next(Source);
      Card := HBlk(HBlk'First);
      CardsCnt := CardsCnt + 1;
      InitList(Card,Keys_To_Parse);
      Parse(Card,Keys_To_Parse,Found_Keys);
      ENDCardFound  := (Card = ENDCard);
      exit when ENDCardFound;

      -- do for 2..end
      for I in (HBlk'First + 1) .. HBlk'Last
      loop
        Card := HBlk(I);
        CardsCnt := CardsCnt + 1;
        AllDataParsed := False; -- FIXME not in use: affects CardsCnt and FileIndex
	Parse(Card,Keys_To_Parse,Found_Keys);
        ENDCardFound  := (Card = ENDCard);
        exit when ENDCardFound OR AllDataParsed;
      end loop;


      -- get other blocks

      HBlk := Next(Source);
      for I in HBlk'Range
      loop
        Card := HBlk(I);
        CardsCnt := CardsCnt + 1;
        AllDataParsed := False; -- FIXME not in use: affects CardsCnt and FileIndex
	Parse(Card,Keys_To_Parse,Found_Keys);
        ENDCardFound  := (Card = ENDCard);
        exit when ENDCardFound OR AllDataParsed;
      end loop;
      exit when ENDCardFound OR AllDataParsed;
    end loop;
    return CardsCnt;
   end wParse_Header;



end FITS.Parser;
