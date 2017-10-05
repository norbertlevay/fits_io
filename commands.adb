
with Ada.Text_IO,-- Ada.Integer_Text_IO,
     Ada.Strings.Fixed,
     Ada.Streams.Stream_IO,
     Ada.Characters.Latin_1,
     GNAT.OS_Lib,
     FITS_SIO,
     System,
     System.Storage_Elements;

use  Ada.Streams.Stream_IO;

package body Commands is

   package TIO renames Ada.Text_IO;

 --
 -- List HDU sizes. For Header: number of cards (and empty slots),
 -- for Data type and axes length
 --
 procedure List_HDUs_In_File (FitsFileName : in String)
 is
  FitsFile : FITS_SIO.SIO.File_Type;

  procedure Print_HDU_Sizes (Index : Positive; HDUInfo : FITS_SIO.HDU_Size_Type)
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
        Ada.Text_IO.Put( Tab & Ada.Strings.Fixed.Head( FITS_SIO.FitsData_Type'Image(HDUInfo.DUSizeParam.Data),8,' ') );
        Ada.Text_IO.Put(" ( ");
        for J in 1 .. (HDUInfo.DUSizeParam.Naxes - 1)
         loop
          Ada.Text_IO.Put(FITS_SIO.FPositive'Image(HDUInfo.DUSizeParam.Naxis(J)) & " x " );
        end loop;
        Ada.Text_IO.Put(Fits_SIO.FPositive'Image(HDUInfo.DUSizeParam.Naxis(HDUInfo.DUSizeParam.Naxes)));
        Ada.Text_IO.Put_Line(" ) ");
       end if;
  end Print_HDU_Sizes;

  procedure Print_Headline is
    Tab : Character := Ada.Characters.Latin_1.HT;
  begin
   Ada.Text_IO.Put_Line ("HDU#" & Tab & " Cards" & Tab & "Data");
  end Print_Headline;


 begin
   FITS_SIO.SIO.Open(FitsFile,FITS_SIO.SIO.In_File,FitsFileName);
   Print_Headline;
   FITS_SIO.List_Content (FitsFile, Print_HDU_Sizes'Access);
   FITS_SIO.SIO.Close(FitsFile);
 end List_HDUs_In_File;

 --
 -- print info on values limited by implementation and/or system
 --
 procedure Limits
 is
  Tab : Character := Ada.Characters.Latin_1.HT;
 begin
  Ada.Text_IO.Put_Line("Limits imposed by implementation and/or the system:");
  Ada.Text_IO.Put_Line("Max NAXIS  :" & Tab & Positive'Image(FITS_SIO.MaxAxes));
  Ada.Text_IO.Put_Line("Max NAXISn :" & Tab & FITS_SIO.FPositive'Image(FITS_SIO.FPositive'Last));
  Ada.Text_IO.Put_Line("Max File size :" & Tab & FITS_SIO.SIO.Positive_Count'Image(FITS_SIO.SIO.Positive_Count'Last));
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
  Ada.Text_IO.Put_Line("Endianness (Bit Order)" & Tab & System.Bit_Order'Image(FITS_SIO.DataArray_Type'Bit_Order));
 end Limits;

 --
 -- output Header from file
 --
 procedure Print_Header( FileName : in String;
                         HDUNum   : in Positive := 1 )
 is
   FitsFile : FITS_SIO.SIO.File_Type;
   Data     : FITS_SIO.DataArray_Type(FITS_SIO.Card , 1);
   ENDCardFound : Boolean := false;
 begin
   FITS_SIO.SIO.Open(FitsFile, FITS_SIO.SIO.In_File, FileName);

   FITS_SIO.Set_Index(FitsFile,HDUNum,Data.FitsType);

   loop
    FITS_SIO.DataArray_Type'Read(FITS_SIO.SIO.Stream(FitsFile) , Data);
    Ada.Text_IO.Put_Line(Data.CardArr(1));
    exit when (Data.CardArr(1) = FITS_SIO.ENDCard);
   end loop;

   FITS_SIO.SIO.Close(FitsFile);

 end Print_Header;

 --
 -- Utils
 --

 -- as example do remove all occurences of card with given keyword InKey
 procedure Remove_Cards_By_Key(InFits  : FITS_SIO.SIO.File_Type;
                               InKey   : String;
                               OutFits : FITS_SIO.SIO.File_Type)
 is
   InData  : FITS_SIO.DataArray_Type(FITS_SIO.HBlock, 1);
   OutData : FITS_SIO.DataArray_Type(FITS_SIO.Card,   1);
   ENDCardFound    : Boolean := false;
   Card            : FITS_SIO.Card_Type;
   CntCardsWritten : Natural := 0;
   CntRemBlock     : Natural; -- remaining empty cards slots in last block
 begin

   loop

     FITS_SIO.DataArray_Type'Read (FITS_SIO.SIO.Stream(InFits), InData);

     for I in InData.HBlockArr(1)'Range
     loop
       Card := InData.HBlockArr(1)(I);

       -- skip Card starting with InKey
       if Card(InKey'Range) /= InKey then
         OutData.CardArr(1) := Card;
         FITS_SIO.DataArray_Type'Write(FITS_SIO.SIO.Stream(OutFits),OutData);
         CntCardsWritten := CntCardsWritten + 1;
       end if;

       ENDCardFound := (Card = FITS_SIO.ENDCard);
       exit when ENDCardFound;
     end loop; -- for loop

     exit when ENDCardFound;
   end loop;

   -- fill upto block limit

   CntRemBlock := CntCardsWritten mod FITS_SIO.CardsCntInBlock;
   if (CntRemBlock > 0) then
    while (CntRemBlock < FITS_SIO.CardsCntInBlock)
    loop
      OutData.CardArr(1) := FITS_SIO.EmptyCard;
      FITS_SIO.DataArray_Type'Write(FITS_SIO.SIO.Stream(OutFits),OutData);
      CntRemBlock := CntRemBlock + 1;
     end loop;
    end if;

 end Remove_Cards_By_Key;

 --
 -- modify the header
 --
 -- it is assumed file-pointers are positioned to the first Char of the Headers
  -- ref: [FITS, Sect 4.4.1 Table 7 Mandatory keywords for primary header.]

 function Find_Card(InFits  : FITS_SIO.SIO.File_Type;
                    InKey   : String ) return FITS_SIO.Card_Type
 is
   Card : FITS_SIO.Card_Type;
   ENDCardFound : Boolean := false;
   KeyCardFound : Boolean := false;
   InData  : FITS_SIO.DataArray_Type(FITS_SIO.HBlock, 1);
--   OutData : FITS_SIO.DataArray_Type(FITS_SIO.Card,   1);
 begin

   loop
     FITS_SIO.DataArray_Type'Read (FITS_SIO.SIO.Stream(InFits), InData);

     for I in InData.HBlockArr(1)'Range
     loop
       Card := InData.HBlockArr(1)(I);

       KeyCardFound := (Card(InKey'Range) = InKey);
       ENDCardFound := (Card = FITS_SIO.ENDCard);

      exit when (KeyCardFound or ENDCardFound);
     end loop;

    exit when (KeyCardFound or ENDCardFound);
   end loop;

   if not KeyCardFound and ENDCardFound then
    null; -- FIXME throw exception probably not a FITS-file ?
   end if;

   return Card;
 end Find_Card;


 procedure Do_Clean_Header_Start(InFits  : FITS_SIO.SIO.File_Type;
                                 OutFits : FITS_SIO.SIO.File_Type)
 is
  -- store HDU start position
  InIdx  : FITS_SIO.SIO.Positive_Count := FITS_SIO.SIO.Index(InFits);
  OutIdx : FITS_SIO.SIO.Positive_Count := FITS_SIO.SIO.Index(OutFits);
  Card   : FITS_SIO.Card_Type;
  NAxis  : Natural;
  NAXISKey : String := "NAXIS";
  ENDCardFound : Boolean := false;
 -- AxisValue : Natural;
  FillCnt  : FITS_SIO.SIO.Count;
  OutIndex : FITS_SIO.SIO.Positive_Count;
 begin

  Card := Find_Card(InFits,"SIMPLE");
  TIO.Put_Line("DBG >>" & Card & "<<");
  String'Write (FITS_SIO.SIO.Stream(OutFits), Card);

  FITS_SIO.SIO.Set_Index(InFits,InIdx);
  Card := Find_Card(InFits,"BITPIX");
  TIO.Put_Line("DBG >>" & Card & "<<");
  String'Write (FITS_SIO.SIO.Stream(OutFits), Card);

  FITS_SIO.SIO.Set_Index(InFits,InIdx);
  Card := Find_Card(InFits,"NAXIS");
  TIO.Put_Line("DBG >>" & Card & "<<");
  String'Write (FITS_SIO.SIO.Stream(OutFits), Card);
  NAxis := Positive'Value(Card(10..30));

  for I in 1 .. NAxis
  loop
    declare
      NAXISnKey : String := NAXISKey & Ada.Strings.Fixed.Trim(Integer'Image(I),Ada.Strings.Left);
    begin
      FITS_SIO.SIO.Set_Index(InFits,InIdx);
      Card := Find_Card(InFits,NAXISnKey);
      String'Write (FITS_SIO.SIO.Stream(OutFits), Card);
      TIO.Put_Line("DBG >>" & Card & "<<");
    end;
  end loop;

  -- write all cards except those above

  FITS_SIO.SIO.Set_Index(InFits,InIdx);

  loop
     String'Read (FITS_SIO.SIO.Stream(InFits), Card);
     if Card(1..6) /= "SIMPLE" and
        Card(1..6) /= "BITPIX" and
        Card(1..5) /= "NAXIS" -- this is not enough there could a card NAXISWHATEVER
     then
        -- more checks on NAXIS: pos 9..10 is '= '
        -- conversion will raise exception if not a number
        -- FIXME we should check also for spaces like 'NAXIS 3 = ' <- see FITS-standard
        -- AxisValue := Positive'Value(Card(6..8));
        String'Write (FITS_SIO.SIO.Stream(OutFits), Card);
        TIO.Put_Line("DBG >>" & Card & "<<");
     end if;

     ENDCardFound := (Card = FITS_SIO.ENDCard);

    exit when ENDCardFound;
   end loop;

   -- fill upto Block limit

   OutIndex := FITS_SIO.SIO.Index(OutFits);
   FillCnt  := (OutIndex - 1) mod 2880;-- FIXME proper conversion to be added
   FillCnt := FillCnt / 80;
   TIO.Put_Line("DBG FillCnt B >>" & FITS_SIO.SIO.Count'Image(FillCnt) & "<<");

   if FillCnt /= 36
   then
    while FillCnt < 36
    loop
        String'Read (FITS_SIO.SIO.Stream(InFits), Card);-- dummy only to move file pointer over fill area
        String'Write (FITS_SIO.SIO.Stream(OutFits), FITS_SIO.EmptyCard);
        FillCnt := FillCnt + 1;
        TIO.Put_Line("DBG FillCnt >>" & FITS_SIO.SIO.Count'Image(FillCnt) & "<<");
    end loop;
   end if;
   -- FIXME fitsverify on broken HiGAL: data fill-area invalid

 end Do_Clean_Header_Start;

 --
 -- make sure order of first mandatory keywords in header
 -- is as [FITS] requires : SIMPLE, BITPIX, NAXIS...NAXISn
 --
 procedure Clean_Header_Start(InFitsName       : in String;
                              OutFitsName      : in String;
                              HDUNum           : in Positive := 1)
 is
   KeyRem  : String := "DATE "; -- key to remove
   InFits  : FITS_SIO.SIO.File_Type;
   OutFits : FITS_SIO.SIO.File_Type;
   Data    : FITS_SIO.DataArray_Type(FITS_SIO.HBlock , 1);
   CurHDUNum : Positive := 1;
   CurSIOIndex    : FITS_SIO.SIO.Positive_Count := 1;
   TargetSIOIndex : FITS_SIO.SIO.Positive_Count;
   ENDCardFound   : Boolean := false;
   NBlocks  : FITS_SIO.FPositive;
   nb       : Natural;
   FileSize : FITS_SIO.SIO.Positive_Count;
 begin

   FITS_SIO.SIO.Create(OutFits, FITS_SIO.SIO.Out_File, OutFitsName);-- FIXME will overwrite ix exits ?
   FITS_SIO.SIO.Open  (InFits,  FITS_SIO.SIO.In_File,  InFitsName);

   -- find position of HDU to be mofied
   -- FIXME ? alternatively: implement
   -- FITS_SIO.Get_HDUSize_blocks() and Copy_HDU(InFitsFile, InHDUNum, OutFits)
   -- and cycle by blocks until HDUSize and cycle by HDU until HDUNum
   FITS_SIO.Set_Index(InFits,  HDUNum, Data.FitsType);
   TargetSIOIndex := FITS_SIO.SIO.Index(InFits);

   -- reset to InFits begining

   FITS_SIO.SIO.Set_Index(OutFits,1);
   FITS_SIO.SIO.Set_Index(InFits, 1);

   -- copy all HDUs upto HDUNum
   TIO.Put_Line("DBG> Index: " & FITS_SIO.SIO.Positive_count'Image(TargetSIOIndex));

   nb := Natural(TargetSIOIndex / 2880);
   if nb /= 0 then
    NBlocks := FITS_SIO.FPositive(nb);-- FIXME all if and conversion
    FITS_SIO.Copy_Blocks (InFits, OutFits, NBlocks, 400);
   end if;

   -- now we are are positioned at HDUNum: modify Header
--   Remove_Cards_By_Key(InFits,KeyRem, OutFits);
   Do_Clean_Header_Start(InFits,OutFits);-- FIXME <-- ds9 show shift, fitsverify says incorrect data fill-up values

   -- copy the rest of the file

   FileSize       := FITS_SIO.SIO.Size(InFits);
   TargetSIOIndex := FITS_SIO.SIO.Index(InFits);

   nb := Natural(((FileSize + 1) - TargetSIOIndex) / 2880);
   if nb /= 0 then
    NBlocks := FITS_SIO.FPositive(nb);-- FIXME all if and conversion
    FITS_SIO.Copy_Blocks (InFits, OutFits, NBlocks, 400);
   end if;

   FITS_SIO.SIO.Close(InFits);
   FITS_SIO.SIO.Close(OutFits);

 end Clean_Header_Start;


end Commands;

