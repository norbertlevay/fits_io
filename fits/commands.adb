
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Streams.Stream_IO;  use Ada.Streams.Stream_IO;
with Ada.Characters.Latin_1;
with System;
with System.Storage_Elements;

with FITS;        use FITS;
with FITS.File;   use FITS.File;
with FITS.File.Misc;   use FITS.File.Misc;
--with FITS.Header; use FITS.Header;


package body Commands is

   package TIO renames Ada.Text_IO;

 --
 -- List HDU sizes. For Header: number of cards (and empty slots),
 -- for Data type and axes length
 --
  procedure Print_Headline is
    Tab : Character := Ada.Characters.Latin_1.HT;
  begin
   TIO.Put_Line ("HDU#" & Tab & "Extension " & Tab & " Cards" & Tab & "Data");
  end Print_Headline;

  procedure Print_HDU_Info (Index   : in Positive;
                            HDUInfo : in HDU_Info_Type)
  is
      FreeSlotCnt : Natural := 0;-- FIXME Free_Card_Slots(HDUInfo.CardsCnt);
      Tab : Character := Ada.Characters.Latin_1.HT;
  begin

       TIO.Put( Ada.Strings.Fixed.Tail(Integer'Image(Index),2,' ') &
                        Tab &
                        -- Integer'Image(HDUInfo.XTENSION'Length) &
                        Max20.To_String(HDUInfo.XTENSION) &
                        Tab &
                        Ada.Strings.Fixed.Tail( FInteger'Image(HDUInfo.CardsCnt),5,' ') &
                        " (" &
                        Ada.Strings.Fixed.Tail(Integer'Image( FreeSlotCnt ),2,' ') &
                        ")" );

       if HDUInfo.NAXISn'Length > 0 then
        TIO.Put( Tab & Ada.Strings.Fixed.Head( Data_Type'Image(To_DataType(HDUInfo.BITPIX)),8,' ') );
        TIO.Put(" ( ");
        for J in 1 .. (HDUInfo.NAXISn'Last - 1)
         loop
          TIO.Put(FPositive'Image(HDUInfo.NAXISn(J)) & " x " );
        end loop;
        TIO.Put(FPositive'Image(HDUInfo.NAXISn(HDUInfo.NAXISn'Last)));
        TIO.Put_Line(" ) ");
       end if;
  end Print_HDU_Info;

 procedure List_HDUs_In_File (FitsFileName : in String)
 is
  FitsFile : SIO.File_Type;
  HDUNum   : Positive := 1;
 begin
   SIO.Open(FitsFile,SIO.In_File,FitsFileName);
   -- see Ada RM: is guaranteed that FileIndex=1 after Open??
   Print_Headline;
   while not SIO.End_Of_File(FitsFile)
    loop
     declare
      HDUInfo : HDU_Info_Type := Get(FitsFile);
     begin
      Print_HDU_Info(HDUNum,HDUInfo);
      HDUNum := HDUNum + 1;
      Set_Index(FitsFile, HDUNum);
     end;
    end loop;
   SIO.Close(FitsFile);
 end List_HDUs_In_File;

 --
 -- print info on values limited by implementation and/or system
 --
 procedure Limits
 is
  Tab : Character := Ada.Characters.Latin_1.HT;
 begin
  TIO.Put_Line("Limits imposed by implementation and/or the system:");
  TIO.Put_Line("Max NAXIS  :" & Tab & Positive'Image(NAXIS_Type'Last));
  TIO.Put_Line("Max NAXISn :" & Tab & FPositive'Image(FPositive'Last));
  TIO.Put_Line("Max File size :" & Tab & SIO.Positive_Count'Image(SIO.Positive_Count'Last));
  TIO.Put_Line("Max DataUnit size :" & Tab & "???" );
  TIO.New_Line;
  TIO.Put_Line("Supported only machines of wordsize not bigger then min(BITPIX)=8 and divisible." );
  TIO.Put_Line("System Name  " & Tab & System.Name'Image(System.System_Name));
  TIO.Put_Line("Storage Element (Byte)" & Tab & System.Storage_Elements.Storage_Element'Image(System.Storage_Elements.Storage_Element'Size) & " [bits]");
  TIO.Put_Line("Storage Unit " & Tab & Integer'Image(System.Storage_Unit) & " [bits]");
  TIO.Put_Line("Word Size (size of pointer/address)    " & Tab & Integer'Image(System.Word_Size) & " [bits]");
  TIO.Put_Line("Address Size " & Tab & Integer'Image(Standard'Address_Size) & " [bits]");
--  TIO.Put_Line("Memory Size  " & Tab & Long_Long_Integer'Image(System.Memory_Size));
   -- Memory_Size : not very useful [Ada]
  TIO.Put_Line("Default Bit Order" & Tab & System.Bit_Order'Image(System.Default_Bit_Order));
--  TIO.Put_Line("Endianness (Bit Order)" & Tab & System.Bit_Order'Image(DataArray_Type'Bit_Order) & " (High_Order_First = BigEndian)");
 end Limits;

 --
 -- output Header from file
 --
 procedure Print_Header( FileName : in String;
                         HDUNum   : in Positive := 1 )
 is
   FitsFile : SIO.File_Type;
   Data     : Card_Type;
   ENDCardFound : Boolean := false;
 begin
   SIO.Open(FitsFile, SIO.In_File, FileName);

   Set_Index(FitsFile,HDUNum);

   loop
    Data := Read_Card(FitsFile);
    TIO.Put_Line(Data);
    exit when (Data = ENDCard);
   end loop;

   SIO.Close(FitsFile);

 end Print_Header;

 --
 -- Utils
 --


 -- as example do remove all occurences of card with given keyword InKey
 -- FIXME not used anywhere??
 procedure Remove_Cards_By_Key(InFits  : SIO.File_Type;
                               InKey   : String;-- FIXME bounded string Max_8
                               OutFits : SIO.File_Type)
 is
   Card : Card_Type;
   PosAfterLastWrite : SIO.Positive_Count;
 begin

   loop
     Card := Read_Card(InFits);
     exit when (Card = ENDCard);

     if Card(InKey'Range) /= InKey then
       Write_Card(OutFits,Card);
     end if;

   end loop;

   Write_Card(OutFits,ENDCard);
   PosAfterLastWrite := SIO.Index(OutFits);
   Write_Padding(OutFits,
                 PosAfterLastWrite,
                 HeaderPadValue);

 end Remove_Cards_By_Key;


 --
 -- modify the header
 --
 -- it is assumed file-pointers are positioned to the first Char of the Headers
  -- ref: [FITS, Sect 4.4.1 Table 7 Mandatory keywords for primary header.]

 -- return all Card if its key is InKey
 -- reads by Block
 function  Find_Card(InFits : SIO.File_Type;
                     InKey  : String ) return Card_Type
 is
   Card : Card_Type;
   ENDCardFound : Boolean := false;
   KeyCardFound : Boolean := false;
 begin

   loop
     Card := Read_Card(InFits);
     KeyCardFound := (Card(InKey'Range) = InKey);
     ENDCardFound := (Card = ENDCard);
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
  Naxis  : Natural;
  NAXISKey : String := "NAXIS";
  PosAfterLastWrite : SIO.Positive_Count;
 begin

  Card := Find_Card(InFits,"SIMPLE");
--  TIO.Put_Line("DBG >>" & Card & "<<");
  Write_Card(OutFits,Card);

  SIO.Set_Index(InFits,InIdx);
  Card := Find_Card(InFits,"BITPIX");
--  TIO.Put_Line("DBG >>" & Card & "<<");
  Write_Card(OutFits,Card);

  SIO.Set_Index(InFits,InIdx);
  Card := Find_Card(InFits,"NAXIS");
--  TIO.Put_Line("DBG >>" & Card & "<<");
  Write_Card(OutFits,Card);

  Naxis := Positive'Value(Card(10..30));

  for I in 1 .. Naxis
  loop
    declare
      NAXISnKey : String := NAXISKey & Ada.Strings.Fixed.Trim(Integer'Image(I),Ada.Strings.Left);
    begin
      SIO.Set_Index(InFits,InIdx);
      Card := Find_Card(InFits,NAXISnKey);
      Write_Card(OutFits,Card);

--      TIO.Put_Line("DBG >>" & Card & "<<");
    end;
  end loop;

  -- write all cards except those above

  SIO.Set_Index(InFits,InIdx);

  loop
    Card := Read_Card(InFits);

    exit when (Card = ENDCard);

    if Card(1..6) /= "SIMPLE" and
       Card(1..8) /= "XTENSION" and
       Card(1..6) /= "BITPIX" and
       Card(1..5) /= "NAXIS" -- this is not enough there could a card NAXISWHATEVER
    then
       -- more checks on NAXIS: pos 9..10 is '= '
       -- conversion will raise exception if not a number
       -- FIXME we should check also for spaces like 'NAXIS 3 = ' <- see FITS-standard
       -- AxisValue := Positive'Value(Card(6..8));
       Write_Card(OutFits,Card);
    end if;

  end loop;

  Write_Card(OutFits,ENDCard);
  PosAfterLastWrite := SIO.Index(OutFits);
  Write_Padding(OutFits,
                PosAfterLastWrite,
                HeaderPadValue);

 end Clean_PrimaryHeader_Start;

 --
 -- Modify HDU copies all HDU's except HDUNum
 -- FIXME For HDUNum use callback instead of Clean_PrimaryHeader_Start
 --
 procedure Copy_File_And_Modify_HDU(InFitsName  : in String;
		                    OutFitsName : in String;
                                    Command     : in HDUCmd_Type;
                                    InKey       : in String; -- FIXME use variant record when params for more commands needed
                		    HDUNum      : in Positive := 1)
 is
   InFits  : SIO.File_Type;
   OutFits : SIO.File_Type;
   CurHDUNum : Positive := 1;
   InIdx  : SIO.Positive_Count;
   DUSize_blocks : FNatural;
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

   -- A, modify (and copy) the Header
   InIdx := SIO.Index(InFits);
   -- store header start position
   case Command is
     when cleanhead =>
       if CurHDUNum = 1 then
        Clean_PrimaryHeader_Start(InFits,OutFits);
        CurHDUNum := CurHDUNum + 1;
       end if;
     when removekey =>
       Remove_Cards_By_Key(InFits, InKey, OutFits);
       CurHDUNum := CurHDUNum + 1;
   end case;
   -- both commands above modify HeaderBlocks
   -- so now file index points to DataUnit start
   -- B, copy the DataUnit (first get the size and then copy)
   SIO.Set_Index(InFits,InIdx);-- position to Header start
   DUSize_blocks := FInteger(DU_Size_blocks (InFits)); -- FIXME explicit conversion
   Copy_Blocks(InFits,OutFits,DUSize_blocks,400);

   -- copy the rest of the file

   while not SIO.End_Of_File(InFits)
   loop
     Copy_HDU(InFits,OutFits,CurHDUNum);
     CurHDUNum := CurHDUNum + 1;
   end loop;

   SIO.Close(InFits);
   SIO.Close(OutFits);

 end Copy_File_And_Modify_HDU;

 --
 -- Writes fill-in data from curent file-index until next Block limit
 -- FIXME consider to implement for Header and also DataUnit padding
 procedure NOT_USED_Write_Fillin (OutFits : in SIO.File_Type)
 is
  OutIndex : SIO.Positive_Count;
  FillCnt  : SIO.Positive_Count;
 begin
   -- FIXME make the below 3 lines FITS_SIO API: procedure Write_Fillin(OutFits,Fill_Value)
   OutIndex := SIO.Index(OutFits);
   FillCnt  := (OutIndex - 1) mod 2880;-- FIXME proper conversion to be added
   FillCnt := FillCnt / 80;
--   TIO.Put_Line("DBG FillCnt B >>" & SIO.Count'Image(FillCnt) & "<<");
   if FillCnt /= 36
   then
    while FillCnt < 36
    loop
        String'Write (SIO.Stream(OutFits), EmptyCard);
        FillCnt := FillCnt + 1;
--        TIO.Put_Line("DBG FillCnt >>" & SIO.Count'Image(FillCnt) & "<<");
    end loop;
   end if;
 end NOT_USED_Write_Fillin;



 procedure FITS_To_PNG (FitsFileName : in String;
                        PngFileName  : in String;
                        HDUNum       : in Positive := 1;
                        PlaneNum     : in Positive := 1) is separate;


end Commands;
