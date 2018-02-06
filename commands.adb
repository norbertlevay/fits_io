
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


with PNG_IO;
use  PNG_IO;

with Interfaces;
use  Interfaces;
-- for PutFITSData debug only


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
      FreeSlotCnt : Natural := Free_Card_Slots(HDUInfo.CardsCnt);
      Tab : Character := Ada.Characters.Latin_1.HT;
      CardsPerBlock_FPos : FPositive := FPositive(CardsCntInBlock);
  begin

       Ada.Text_IO.Put( Integer'Image(Index) &
                        Tab &
                        HDUInfo.XTENSION &
                        Tab &
                        Ada.Strings.Fixed.Tail( FInteger'Image(HDUInfo.CardsCnt),5,' ') &
                        " (" &
                        Ada.Strings.Fixed.Tail(Integer'Image( FreeSlotCnt ),2,' ') &
                        ")" );

       if HDUInfo.DUSizeKeyVals.NAXIS > 0 then
        Ada.Text_IO.Put( Tab & Ada.Strings.Fixed.Head( FitsData_Type'Image(To_FitsDataType(HDUInfo.DUSizeKeyVals.BITPIX)),8,' ') );
        Ada.Text_IO.Put(" ( ");
        for J in 1 .. (HDUInfo.DUSizeKeyVals.NAXIS - 1)
         loop
          Ada.Text_IO.Put(FPositive'Image(HDUInfo.DUSizeKeyVals.NAXISn(J)) & " x " );
        end loop;
        Ada.Text_IO.Put(FPositive'Image(HDUInfo.DUSizeKeyVals.NAXISn(HDUInfo.DUSizeKeyVals.NAXIS)));
        Ada.Text_IO.Put_Line(" ) ");
       end if;
  end Print_HDU_Sizes;

  procedure Print_Headline is
    Tab : Character := Ada.Characters.Latin_1.HT;
  begin
   Ada.Text_IO.Put_Line ("HDU#" & Tab & "Extension " & Tab & " Cards" & Tab & "Data");
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

   Set_Index(FitsFile,HDUNum);

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
 -- read by HBlock's &write by Card's
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

 -- return all Card if its key is InKey
 -- reads by Block
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
  FillCnt  : Positive;
--  DUSize_blocks : FNatural;
  InData  : DataArray_Type(HBlock, 1);
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

  -- read by Blocks, write by Cards
  loop
    DataArray_Type'Read (SIO.Stream(InFits), InData);

    for I in InData.HBlockArr(1)'Range
    loop
       Card := InData.HBlockArr(1)(I);

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
--          TIO.Put_Line("DBG >>" & Card & "<<");
       end if;

       ENDCardFound := (Card = ENDCard);
       FillCnt := I;
       exit when ENDCardFound;
    end loop;

    exit when ENDCardFound;
  end loop;

   -- fill upto Block limit

--   TIO.Put_Line("DBG FillCnt B >>" & SIO.Count'Image(FillCnt) & "<<");
   while FillCnt < 36
   loop
        String'Write (SIO.Stream(OutFits), EmptyCard);
        FillCnt := FillCnt + 1;
--        TIO.Put_Line("DBG FillCnt >>" & SIO.Count'Image(FillCnt) & "<<");
   end loop;

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
   SIO.Set_Index(InFits,InIdx);
   DUSize_blocks := DU_Size_blocks (InFits);
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
 procedure Write_Fillin (OutFits : in SIO.File_Type)
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
 end Write_Fillin;

 -- convert FITS to PNG image
 -- how to handle more then 2D files ?
 procedure FITS_To_PNG (FitsFileName : in String)
 is
  HDUNum  : Positive := 1;--Integer'Value( Argument(1) );
  FitsFile    : SIO.File_Type;
  PngFileName : String := FitsFileName & ".png";
  HDUSize : HDU_Size_Type;

  procedure PutFITSData (Data : in DataArray_Type)
  is
  begin
   for I in Positive range 1 .. Data.Length
   loop
    case Data.FitsType is
    when Int8 =>
     Ada.Text_IO.Put( Interfaces.Integer_8'Image(Data.Int8Arr(I)) & " ");
    when Int16 =>
     Ada.Text_IO.Put( Interfaces.Integer_16'Image(Data.Int16Arr(I)) & " ");
    when Int32 =>
     Ada.Text_IO.Put( Interfaces.Integer_32'Image(Data.Int32Arr(I)) & " ");
    when Int64 =>
     Ada.Text_IO.Put( Interfaces.Integer_64'Image(Data.Int64Arr(I)) & " ");
    when Float32 =>
     Ada.Text_IO.Put( Interfaces.IEEE_Float_32'Image(Data.Float32Arr(I)) & " ");
    when Float64 =>
     Ada.Text_IO.Put( Interfaces.IEEE_Float_64'Image(Data.Float64Arr(I)) & " ");
    when others =>
      null; -- FIXME exception or ?
    end case;
   end loop;
   Ada.Text_IO.New_Line;
  end PutFITSData;

  type My_Image_Handle is array (Natural range <>, Natural range <>) of Natural;

 begin
  -- -----------------
  -- read FITS file:
  --
  SIO.Open(FitsFile,SIO.In_File,FitsFileName);

--  Set_Index(FitsFile,HDUNum);
  Parse_HeaderBlocks(FitsFile,HDUSize);-- move behind the Header
  Ada.Text_IO.Put_Line("DU start: " & Count'Image(Index(FitsFile)));
  Ada.Text_IO.Put_Line("DU start: " & Count'Image(Index(FitsFile)+1));

  Set_Index(FitsFile,Index(FitsFile)+1);-- WHY +1 FIXME !!!!

  declare
     dt    : FitsData_Type := To_FitsDataType(HDUSize.DUSizeKeyVals.BITPIX);
     W : constant Dimension        := Integer(HDUSize.DUSizeKeyVals.NAXISn(1));
     H : constant Dimension        := Integer(HDUSize.DUSizeKeyVals.NAXISn(2));
                                  -- FIXME explicit cast!
     F : My_Image_Handle(0..W-1,0..H-1);-- := (others => 127);
        -- holds data for PNG image write

     DataD : DataArray_Type( dt, W*H );
     wi    : Natural := 0;
     hi    : Natural := 0;
  begin
     Ada.Text_IO.Put_Line("DU type: " & FitsData_Type'Image(dt));

      DataArray_Type'Read (SIO.Stream(FitsFile), DataD);

     for dd of DataD.Float32Arr
     loop

       begin

        dd := dd * 127.0/2000.0; -- with WFPC2ASSNu5780205bx.fits

        if (dd >= 0.0) and (dd <= 127.0) then
         F(wi,hi) := Standard.Natural(dd);
         -- Ada.Text_IO.Put(" [" & Natural'Image(wi));
         -- Ada.Text_IO.Put(" " & Natural'Image(hi));
         -- Ada.Text_IO.Put("] " & Interfaces.IEEE_Float_32'Image(dd));
         -- Ada.Text_IO.Put_Line(" " & Natural'Image(F(wi,hi)));
        elsif (dd<0.0) then
         F(wi,hi) := 0;
        else
         F(wi,hi) := 127;
         -- Ada.Text_IO.Put_Line(" Clip127 " & Interfaces.IEEE_Float_32'Image(dd));
        end if;
       exception
        when Constraint_Error =>
         -- Ada.Text_IO.Put_Line(Interfaces.IEEE_Float_32'Image(dd));
         F(wi,hi) := 0;
       end;

       if wi = W-1 then
        hi := hi + 1;
        wi := 0;
       else
        wi := wi + 1;
       end if;

     end loop;
     Ada.Text_IO.New_Line;

   -- -----------------
   -- write PNG file:
   --
   -- requires instantiation of generic Write_PNG_Type_0() func from PNG_IO.ads
   -- needs 3 things:
   --   something what holds the pixels -> Image_Handle - below not used
   --   type of one pixel/Sample -> below Natural
   --   written function which returns each pixel by coordinates from the Image_Handle
   -- Read the long comment in: png_io.ads l.367 before generic decl of Write_PNG_Type_0
   declare

    function My_Grey_Sample(I    : My_Image_Handle;
                            R, C : Coordinate) return Natural is
      begin
       return I(R,C);
      end My_Grey_Sample;

    procedure Write_0 is new Write_PNG_Type_0(My_Image_Handle, Natural, My_Grey_Sample);

   begin
    Write_0(PngFileName, F, W, H); --, D, I, L); Last 3 params have defaults
   end;
   -- END write PNG file
   -- ------------------

   SIO.Close(FitsFile);

  end; -- 1st declare

 end FITS_To_PNG;

end Commands;
