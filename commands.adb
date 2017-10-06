
with Ada.Text_IO,-- Ada.Integer_Text_IO,
     Ada.Strings.Fixed,
     Ada.Streams.Stream_IO,
     Ada.Characters.Latin_1,
     GNAT.OS_Lib,
     FITS,
     FITS.File,
     System,
     System.Storage_Elements;

use  Ada.Streams.Stream_IO,
     FITS,
     FITS.File;


package body Commands is

   package TIO renames Ada.Text_IO;

 --
 -- List HDU sizes. For Header: number of cards (and empty slots),
 -- for Data type and axes length
 --
 procedure List_HDUs_In_File (FitsFileName : in String)
 is
  FitsFile : SIO.File_Type;

  procedure Print_HDU_Sizes (Index : Positive; HDUInfo : HDU_Size_Type)
  is
      FreeSlotCnt : Natural;
      Tab : Character := Ada.Characters.Latin_1.HT;
  begin
       -- calc free slots
       FreeSlotCnt := 36 - (HDUInfo.CardsCnt mod 36);
       -- mod is 0 when Block has 36 cards e.g. is full
       if FreeSlotCnt = 36 then
        FreeSlotCnt := 0;
       end if;

       Ada.Text_IO.Put( Integer'Image(Index) &
                        Tab &
                        Ada.Strings.Fixed.Tail( Integer'Image(HDUInfo.CardsCnt),5,' ') &
                        " (" &
                        Ada.Strings.Fixed.Tail(Integer'Image( FreeSlotCnt ),2,' ') &
                        ")" );

       if HDUInfo.DUSizeParam.Naxes > 0 then
        Ada.Text_IO.Put( Tab & Ada.Strings.Fixed.Head( FitsData_Type'Image(HDUInfo.DUSizeParam.Data),8,' ') );
        Ada.Text_IO.Put(" ( ");
        for J in 1 .. (HDUInfo.DUSizeParam.Naxes - 1)
         loop
          Ada.Text_IO.Put(FPositive'Image(HDUInfo.DUSizeParam.Naxis(J)) & " x " );
        end loop;
        Ada.Text_IO.Put(FPositive'Image(HDUInfo.DUSizeParam.Naxis(HDUInfo.DUSizeParam.Naxes)));
        Ada.Text_IO.Put_Line(" ) ");
       end if;
  end Print_HDU_Sizes;

  procedure Print_Headline is
    Tab : Character := Ada.Characters.Latin_1.HT;
  begin
   Ada.Text_IO.Put_Line ("HDU#" & Tab & " Cards" & Tab & "Data");
  end Print_Headline;


 begin
   SIO.Open(FitsFile,SIO.In_File,FitsFileName);
   Print_Headline;
   List_Content (FitsFile, Print_HDU_Sizes'Access);
   SIO.Close(FitsFile);
 end List_HDUs_In_File;

 --
 -- print info on values limited by implementation and/or system
 --
 procedure Limits
 is
  Tab : Character := Ada.Characters.Latin_1.HT;
 begin
  Ada.Text_IO.Put_Line("Limits imposed by implementation and/or the system:");
  Ada.Text_IO.Put_Line("Max NAXIS  :" & Tab & Positive'Image(MaxAxes));
  Ada.Text_IO.Put_Line("Max NAXISn :" & Tab & FPositive'Image(FPositive'Last));
  Ada.Text_IO.Put_Line("Max File size :" & Tab & SIO.Positive_Count'Image(SIO.Positive_Count'Last));
  Ada.Text_IO.Put_Line("Max DataUnit size :" & Tab & "???" );
  Ada.Text_IO.New_Line;
  Ada.Text_IO.Put_Line("Supported only machines of wordsize not bigger then min(BITPIX)=8 and divisible." );
  Ada.Text_IO.Put_Line("System Name  " & Tab & System.Name'Image(System.System_Name));
  Ada.Text_IO.Put_Line("Storage Element (Byte)" & Tab & System.Storage_Elements.Storage_Element'Image(System.Storage_Elements.Storage_Element'Size) & " [bits]");
  Ada.Text_IO.Put_Line("Storage Unit " & Tab & Integer'Image(System.Storage_Unit) & " [bits]");
  Ada.Text_IO.Put_Line("Word Size (size of pointer/address)    " & Tab & Integer'Image(System.Word_Size) & " [bits]");
  Ada.Text_IO.Put_Line("Address Size " & Tab & Integer'Image(Standard'Address_Size) & " [bits]");
--  Ada.Text_IO.Put_Line("Memory Size  " & Tab & Long_Long_Integer'Image(System.Memory_Size));
   -- Memory_Size : not very useful [Ada]
  Ada.Text_IO.Put_Line("Default Bit Order" & Tab & System.Bit_Order'Image(System.Default_Bit_Order));
  Ada.Text_IO.Put_Line("Endianness (Bit Order)" & Tab & System.Bit_Order'Image(DataArray_Type'Bit_Order));
 end Limits;

 --
 -- output Header from file
 --
 procedure Print_Header( FileName : in String;
                         HDUNum   : in Positive := 1 )
 is
   FitsFile : SIO.File_Type;
   Data     : DataArray_Type(Card , 1);
   ENDCardFound : Boolean := false;
 begin
   SIO.Open(FitsFile, SIO.In_File, FileName);

   Set_Index(FitsFile,HDUNum,Data.FitsType);

   loop
    DataArray_Type'Read(SIO.Stream(FitsFile) , Data);
    Ada.Text_IO.Put_Line(Data.CardArr(1));
    exit when (Data.CardArr(1) = ENDCard);
   end loop;

   SIO.Close(FitsFile);

 end Print_Header;

 --
 -- Utils
 --

 -- as example do remove all occurences of card with given keyword InKey
 procedure Remove_Cards_By_Key(InFits  : SIO.File_Type;
                               InKey   : String;
                               OutFits : SIO.File_Type)
 is
   InData  : DataArray_Type(HBlock, 1);
   OutData : DataArray_Type(Card,   1);
   ENDCardFound    : Boolean := false;
   Card            : Card_Type;
   CntCardsWritten : Natural := 0;
   CntRemBlock     : Natural; -- remaining empty cards slots in last block
 begin

   loop

     DataArray_Type'Read (SIO.Stream(InFits), InData);

     for I in InData.HBlockArr(1)'Range
     loop
       Card := InData.HBlockArr(1)(I);

       -- skip Card starting with InKey
       if Card(InKey'Range) /= InKey then
         OutData.CardArr(1) := Card;
         DataArray_Type'Write(SIO.Stream(OutFits),OutData);
         CntCardsWritten := CntCardsWritten + 1;
       end if;

       ENDCardFound := (Card = ENDCard);
       exit when ENDCardFound;
     end loop; -- for loop

     exit when ENDCardFound;
   end loop;

   -- fill upto block limit

   CntRemBlock := CntCardsWritten mod CardsCntInBlock;
   if (CntRemBlock > 0) then
    while (CntRemBlock < CardsCntInBlock)
    loop
      OutData.CardArr(1) := EmptyCard;
      DataArray_Type'Write(SIO.Stream(OutFits),OutData);
      CntRemBlock := CntRemBlock + 1;
     end loop;
    end if;

 end Remove_Cards_By_Key;

 --
 -- modify the header
 --
 -- it is assumed file-pointers are positioned to the first Char of the Headers
  -- ref: [FITS, Sect 4.4.1 Table 7 Mandatory keywords for primary header.]

 function  Find_Card(InFits : SIO.File_Type;
                     InKey  : String ) return Card_Type
 is
   Card : Card_Type;
   ENDCardFound : Boolean := false;
   KeyCardFound : Boolean := false;
   InData  : DataArray_Type(HBlock, 1);
 begin

   loop
     DataArray_Type'Read (SIO.Stream(InFits), InData);

     for I in InData.HBlockArr(1)'Range
     loop
       Card := InData.HBlockArr(1)(I);

       KeyCardFound := (Card(InKey'Range) = InKey);
       ENDCardFound := (Card = ENDCard);

      exit when (KeyCardFound or ENDCardFound);
     end loop;

    exit when (KeyCardFound or ENDCardFound);
   end loop;

   if not KeyCardFound and ENDCardFound then
    Card := EmptyCard; -- FIXME 'harmless' pass-by
    null; -- FIXME throw exception probably not a FITS-file ?
   end if;

   return Card;
 end Find_Card;


 --
 -- Mandatory keywords' order in Header is fixed by standard
 -- This func writes them to OutFits in right order.
 -- (Extensions not supported : those have different
 -- set of manadatory keywords for each ext type.)
 --
 procedure Clean_PrimaryHeader_Start(InFits  : SIO.File_Type;
                                     OutFits : SIO.File_Type)
 is
  -- store HDU start position
  InIdx  : SIO.Positive_Count := SIO.Index(InFits);
  OutIdx : SIO.Positive_Count := SIO.Index(OutFits);
  Card   : Card_Type;
  NAxis  : Natural;
  NAXISKey : String := "NAXIS";
  ENDCardFound : Boolean := false;
  FillCnt  : SIO.Count;
  OutIndex : SIO.Positive_Count;
  DUSize_blocks : FNatural;
 begin

  Card := Find_Card(InFits,"SIMPLE");
--  TIO.Put_Line("DBG >>" & Card & "<<");
  String'Write (SIO.Stream(OutFits), Card);

  SIO.Set_Index(InFits,InIdx);
  Card := Find_Card(InFits,"BITPIX");
--  TIO.Put_Line("DBG >>" & Card & "<<");
  String'Write (SIO.Stream(OutFits), Card);

  SIO.Set_Index(InFits,InIdx);
  Card := Find_Card(InFits,"NAXIS");
--  TIO.Put_Line("DBG >>" & Card & "<<");
  String'Write (SIO.Stream(OutFits), Card);
  NAxis := Positive'Value(Card(10..30));

  for I in 1 .. NAxis
  loop
    declare
      NAXISnKey : String := NAXISKey & Ada.Strings.Fixed.Trim(Integer'Image(I),Ada.Strings.Left);
    begin
      SIO.Set_Index(InFits,InIdx);
      Card := Find_Card(InFits,NAXISnKey);
      String'Write (SIO.Stream(OutFits), Card);
--      TIO.Put_Line("DBG >>" & Card & "<<");
    end;
  end loop;

  -- write all cards except those above

  SIO.Set_Index(InFits,InIdx);

  loop
     String'Read (SIO.Stream(InFits), Card);
     if Card(1..6) /= "SIMPLE" and
        Card(1..8) /= "XTENSION" and
        Card(1..6) /= "BITPIX" and
        Card(1..5) /= "NAXIS" -- this is not enough there could a card NAXISWHATEVER
     then
        -- more checks on NAXIS: pos 9..10 is '= '
        -- conversion will raise exception if not a number
        -- FIXME we should check also for spaces like 'NAXIS 3 = ' <- see FITS-standard
        -- AxisValue := Positive'Value(Card(6..8));
        String'Write (SIO.Stream(OutFits), Card);
--        TIO.Put_Line("DBG >>" & Card & "<<");
     end if;

     ENDCardFound := (Card = ENDCard);

    exit when ENDCardFound;
   end loop;

   -- fill upto Block limit

   -- FIXME make the below 3 lines FITS_SIO API: procedure Write_Fillin(OutFits,Fill_Value)
   OutIndex := SIO.Index(OutFits);
   FillCnt  := (OutIndex - 1) mod 2880;-- FIXME proper conversion to be added
   FillCnt := FillCnt / 80;
--   TIO.Put_Line("DBG FillCnt B >>" & SIO.Count'Image(FillCnt) & "<<");
   if FillCnt /= 36
   then
    while FillCnt < 36
    loop
        String'Read  (SIO.Stream(InFits),  Card); -- dummy only to move file pointer over fill area
        String'Write (SIO.Stream(OutFits), EmptyCard);
        FillCnt := FillCnt + 1;
--        TIO.Put_Line("DBG FillCnt >>" & SIO.Count'Image(FillCnt) & "<<");
    end loop;
   end if;

   -- calc size of DU and copy it:
   SIO.Set_Index(InFits,InIdx);
   DUSize_blocks := DU_Size_blocks (InFits);
   Copy_Blocks(InFits,OutFits,DUSize_blocks,400);

 end Clean_PrimaryHeader_Start;

 --
 -- Modify HDU copies all HDU's except HDUNum
 -- FIXME For HDUNum use callback instead of Clean_PrimaryHeader_Start
 --
 procedure Copy_File_And_Modify_HDU(InFitsName  : in String;
		                    OutFitsName : in String;
                		    HDUNum      : in Positive := 1)
 is
   InFits  : SIO.File_Type;
   OutFits : SIO.File_Type;
   CurHDUNum : Positive := 1;
 begin

   SIO.Create(OutFits, SIO.Out_File, OutFitsName);-- FIXME will overwrite ix exits ?
   SIO.Open  (InFits,  SIO.In_File,  InFitsName);

   -- copy all HDUs upto HDUNum

   while CurHDUNum < HDUNum
   loop
     Copy_HDU(InFits,OutFits,CurHDUNum,400);
     CurHDUNum := CurHDUNum + 1;
   end loop;

   -- now we are are positioned at HDUNum:
   -- modify (and copy) the HDU
   if CurHDUNum = 1 then
    Clean_PrimaryHeader_Start(InFits,OutFits);
    CurHDUNum := CurHDUNum + 1;
   end if;

   -- copy the rest of the file

   while not SIO.End_Of_File(InFits)
   loop
     Copy_HDU(InFits,OutFits,CurHDUNum);
     CurHDUNum := CurHDUNum + 1;
   end loop;

   SIO.Close(InFits);
   SIO.Close(OutFits);

 end Copy_File_And_Modify_HDU;


end Commands;

