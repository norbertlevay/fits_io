

with Ada.Text_IO; -- for debug only
with Ada.Integer_Text_IO;

with Ada.Streams.Stream_IO;
use  Ada.Streams.Stream_IO;

with Ada.Strings;
with Ada.Strings.Fixed;

package body HDU is

   subtype Card_Type is String(1..CardSize); -- makes sure index start with 1
   ENDCard  : Card_Type := "END                                                                             ";

   BlockSize       : constant Positive := 2880;
   CardsCntInBlock : constant Positive := BlockSize / CardSize; -- 36 cards per block

   type HeaderBlock_Type  is array (1 .. CardsCntInBlock) of String(1..CardSize);
   EmptyCard  : constant String(1..CardSize) := (others => ' ');
   EmptyBlock : constant HeaderBlock_Type := (others => EmptyCard);
   type HeaderBlocks_Type is array (Positive range <> ) of HeaderBlock_Type;
   -- Header format inside file

   -- set up a buffer for reading fits-file by blocks
   subtype Block_Type is String(1..BlockSize);

   MaxNAXIS : constant Positive := 999;-- [FITS standard Sect 4.4.1 ]
   type AxisArray_Type is array (1..MaxNAXIS) of Natural;
   type Axes_Type is record
    BitPix : Integer; -- FIXME actually fixed values only
    Naxes  : Natural;
    Naxis  : AxisArray_Type;
   end record;
   Null_AxisArray : AxisArray_Type := (others=> 1);
   Null_Axes : Axes_Type := (0,0, Null_AxisArray);
   -- Axis dimesions allow to calculate Data Unit size and position to next HDU in file

 procedure Assert_HDU_Positions_Valid ( HDU : in HDU_Position_Type ) is
  begin
   null;
   -- TODO not implemented: raise excpetion if HDU "not opened"
   -- e.g. HDU_Position_Type was not initialized
   -- should be first call in each func using HDU
  end Assert_HDU_Positions_Valid;

   -- for debug
 procedure Print_AxesDimensions(HDU_Num        : in Positive;
                                AxesDimensions : in Axes_Type) is
  begin
      Ada.Text_IO.Put("HDU#" & Integer'Image(HDU_Num) & "> ");
      Ada.Text_IO.Put(Integer'Image(AxesDimensions.Naxes)  &" : ");
      Ada.Text_IO.Put(Integer'Image(AxesDimensions.BitPix));
      for I in 1 .. AxesDimensions.Naxes
      loop
        Ada.Text_IO.Put(" x " & Integer'Image(AxesDimensions.Naxis(I)));
      end loop;
      Ada.Text_IO.New_Line;
  end Print_AxesDimensions;
  -- for debug
 procedure Print_HeaderBlocks( HB: HeaderBlocks_Type ) is
  begin
   for I in HB'Range loop
    for J in HB(I)'Range loop
        Ada.Integer_Text_IO.Put( I, 5 );
        Ada.Integer_Text_IO.Put( J, 5 );
        Ada.Text_IO.Put_Line(  " >"  & HB(I)(J)  & "<");
    end loop;
   end loop;
  end Print_HeaderBlocks;

   --
   --
   --
   function Calc_DataUnit_Size( AxesDimensions : in Axes_Type )
    return Natural
   is
    DUSize : Natural := 0;
   begin

     if AxesDimensions.Naxes > 0 then
       DUSize := 1;
       for I in 1..AxesDimensions.Naxes loop
        DUSize := DUSize * AxesDimensions.Naxis(I);
       end loop;
       DUSize := DUSize * (abs AxesDimensions.BitPix/8);
       DUSize := BlockSize*(1 + DUSize/BlockSize);
       -- must be multiple of BlockSize
     end if;

    return DUSize;
   end Calc_DataUnit_Size;

   --
   -- parse keywords needed to calculate DU size
   --
   procedure Parse_KeyRecord( Card : Card_Type;
                              AxesDimensions  : in out Axes_Type )
   is
    dim : Positive;
   begin
     -- pos 9..10 is '= '
     -- pos 31 is comment ' /'
     -- then : pos 10..20 is value
     if    (Card(1..9) = "BITPIX  =") then
       AxesDimensions.BitPix := Integer'Value(Card(10..30));
     elsif (Card(1..5) = "NAXIS") then

       if (Card(1..9) = "NAXIS   =") then
           AxesDimensions.Naxes := Positive'Value(Card(10..30));
       else
           dim := Positive'Value(Card(6..8));
           AxesDimensions.Naxis(dim) := Positive'Value(Card(10..30));
       end if;
       -- FIXME what to do if NAXIS and NAXISnn do not match -> see standard

     end if;
   end Parse_KeyRecord;

   --
   -- walk through each HeaderBlock
   --
   function Parse_HeaderBlock( Block   : in Block_Type ;
                               AxesDimensions     : in out Axes_Type ) return Boolean
   is
    ENDFound : Boolean := False;
    Card     : Card_Type;
    CardCnt  : Positive := 1;
    from     : Positive := 1;
    next     : Positive := 1;
   begin
    loop
      next := from + CardSize;
      Card := Block( from .. (next - 1) );
      Parse_KeyRecord( Card, AxesDimensions );
      ENDFound := (Card = ENDCard);
      exit when ENDFound or CardCnt >= CardsCntInBlock;
      CardCnt := CardCnt + 1;
      from := next;
    end loop;
    return ENDFound;
   end Parse_HeaderBlock;

 --
 -- for Open - using existing HDU
 --
 function Parse_HDU_Positions ( FitsFile : in File_Type;
                                HDU_Num  : in Positive )
                                return HDU_Position_Type
 is
   InFitsStreamAccess  : Stream_Access := Stream(FitsFile);
   HDU_Pos : HDU_Position_Type;

   -- controlling the loops
   HDU_Cnt : Positive := 1;
   EndCardFound : Boolean := false;

   -- positions & size in file
   HeadStart_Index : Positive_Count;
   DataStart_Index : Positive_Count;
   DataUnit_Size   : Natural := 0;

   AxesDimensions : Axes_Type;
   Block : Block_Type;
 begin

   while not End_OF_File(FitsFile)
         and (HDU_Cnt <= HDU_Num)
   loop

      AxesDimensions := Null_Axes;

      -- Header

      HeadStart_Index := Index(FitsFile);

      loop
         Block_Type'Read(InFitsStreamAccess,Block);
         EndCardFound := Parse_HeaderBlock( Block , AxesDimensions );
         exit when EndCardFound ;
      end loop;

      -- Print_AxesDimensions(HDU_Cnt,AxesDimensions);

      -- DataUnit

      DataStart_Index := Index(FitsFile);
      DataUnit_Size   := Calc_DataUnit_Size( AxesDimensions );
      Set_Index( FitsFile, DataStart_Index + Count(DataUnit_Size) );-- FIXME DataUnit_Size can be 0 !! Count is >=0 Positive_Count is >0 in Ada.Streams.Stream_IO
      -- skip data unit for next header
                                                              

      HDU_Cnt := HDU_Cnt + 1;
      -- next HDU

   end loop;

   HDU_Pos.Header_Index := Positive(HeadStart_Index);
   HDU_Pos.Header_Size  := Natural(DataStart_Index - HeadStart_Index);
   HDU_Pos.Data_Index   := Positive(DataStart_Index);
   HDU_Pos.Data_Size    := DataUnit_Size;

  return HDU_Pos;

 end Parse_HDU_Positions;

 --
 -- for Put - creating new HDU from Header
 --
 function Parse_HDU_Positions ( HeaderBlocks : in HeaderBlocks_Type;
                                Cur_Index : in Positive_Count )
                                
  return HDU_Position_Type
 is
  HDU_Pos : HDU_Position_Type;
  AxesDimensions : Axes_Type;
  DU_Size : Natural := 0;
  Card : String(1..CardSize);
  ENDFound : Boolean := False;
 begin
   for I in HeaderBlocks'Range
    loop
     for J in HeaderBlocks(I)'Range
      loop
       Card := HeaderBlocks(I)(J);
       Parse_KeyRecord( Card, AxesDimensions );
       ENDFound := (Card = ENDCard);
     end loop;
     exit when ENDFound;
   end loop;

   DU_Size := Calc_DataUnit_Size( AxesDimensions );

   HDU_Pos.Header_Index := Positive(Cur_Index);
   HDU_Pos.Header_Size  := Natural(HeaderBlocks'Length * BlockSize); -- FIXME verify
   HDU_Pos.Data_Index   := Positive(HDU_Pos.Header_Index + HDU_Pos.Header_Size);--FIXME verify this
   HDU_Pos.Data_Size    := DU_Size;

    return HDU_Pos;
 end;


 -- Interface

 function To_File_Mode ( HDUMode : HDU_Mode ) return Ada.Streams.Stream_IO.File_Mode
 is
  FileMode : Ada.Streams.Stream_IO.File_Mode;
 begin

  case HDUMode is
    when In_HDU    => FileMode := Ada.Streams.Stream_IO.In_File;
    when Out_HDU   => FileMode := Ada.Streams.Stream_IO.Out_File;
    when Inout_HDU => FileMode := Ada.Streams.Stream_IO.Out_File;-- [GNAT] Set_Mode switch will open in "r+" Inout_File
    when Append_HDU => FileMode := Ada.Streams.Stream_IO.Append_File;
  when others =>
   null;
   -- raise exception FIXME
  end case;

  return FileMode;
 end To_File_Mode;

 procedure Create ( HDU : in out HDU_Type;
                    Mode : in HDU_Mode;
                    Name : in String;
                    HDU_Num : Positive  := 1;-- Primary HDU
                    Form : in String    := "") is
 -- can be first HDU when FitsFile in Out_Mode, or
 -- last HDU of existing files when FitsFile in Append_Mode
 begin
  Create( HDU.FitsFile, To_File_Mode(Mode), Name, Form);
  HDU.Positions := HDU_Null;
  -- init only to undef
  -- later first write will setup numbers based on given Header
 end;



 -- HDU.FitsFile  := FitsFile; -- FIXME File_Type is limited
 -- e.g. cannot be assigned, can be manipulated only by funcs defined for it...
 procedure Open ( HDU : in out HDU_Type;
                  Mode : in HDU_Mode;
                  Name : in String;
                  HDU_Num : Positive := 1;-- Primary HDU
                  Form : in String   := "") is
 begin
  -- Parse requires read access
  Open(HDU.FitsFile, In_File, Name, Form);
  HDU.Positions := Parse_HDU_Positions ( HDU.FitsFile,
                                         HDU_Num );
  Set_Mode(HDU.FitsFile,To_File_Mode(Mode));
 end;



 procedure Close  ( HDU : in out HDU_Type ) is
 begin
  Close (HDU.FitsFile);
  HDU.Positions := HDU_Null;
 end;

 -- Header conversions: internal (HeaderBlocks_Type) <-> external (Header_Type)

 function CardsCount (HeaderBlocks : HeaderBlocks_Type) return Positive
 is
  Count : Positive := 1;
 begin
  for I in HeaderBlocks'Range loop
   for J in HeaderBlocks(I)'Range loop
    exit when HeaderBlocks(I)(J) = ENDCard;
    Count := Count + 1;
   end loop;
  end loop;
  return Count;
 end CardsCount;
 -- FIXME we need CardsCount stored in the HeaderBlocks_Type

 -- strip empty spaces from start end end of each line
 -- strip empty cards after END-card
 function To_Header( HeaderBlocks : HeaderBlocks_Type ) return Header_Type
 is
  H  : Header_Type(1 .. CardsCount(HeaderBlocks));
  Ix : Positive := 1;
 begin
  for I in HeaderBlocks'Range loop
   for J in HeaderBlocks(I)'Range loop
    H(Ix) := SB.Trim(SB.To_Bounded_String(HeaderBlocks(I)(J)) , Ada.Strings.Both);
    exit when HeaderBlocks(I)(J) = ENDCard;
    Ix := Ix + 1;
   end loop;
  end loop;
  return H;
 end To_Header;

 function To_HeaderBlocks( Header : Header_Type ) return HeaderBlocks_Type
 is
  -- init blocks with space-characters
  HB : HeaderBlocks_Type(1 .. (1 + (Header'Length -1) / CardsCntInBlock)) := (others => EmptyBlock);
  Ix : Positive := 1;-- index in header
  BIx,Cix : Natural;-- block-index, card-index
 begin
  for I in Header'Range loop
    BIx := 1 + (Ix-1) /  CardsCntInBlock;
    CIx := Ix mod CardsCntInBlock;
    if CIx = 0 then
     CIx := CardsCntInBlock;
    end if;
    declare
     str : String := SB.To_String(SB.Trim(Header(I), Ada.Strings.Both));
    begin
     for J in str'Range loop
      HB(BIx)(CIx)(J) := str(J);
     end loop;
     -- FIXME is there a better way?
    end;
    Ix := Ix + 1;
  end loop;
  -- debug Print_HeaderBlocks(HB);
  return HB;
 end To_HeaderBlocks;


 -- Header access

 function Read ( HDU : in HDU_Type )
  return HeaderBlocks_Type is
   InFitsStreamAccess  : Stream_Access := Stream(HDU.FitsFile);
   HeaderBlocks : HeaderBlocks_Type(1..Integer(HDU.Positions.Header_Size)/BlockSize);
  begin
   Set_Index(HDU.FitsFile, Positive_Count(HDU.Positions.Header_Index));
   HeaderBlocks_Type'Read(InFitsStreamAccess,HeaderBlocks);
   return HeaderBlocks;
  end Read;

 function Read ( HDU : in HDU_Type )
  return Header_Type is
  begin
   return To_Header(Read(HDU));
  end Read;


 procedure Write ( HDU          : in out HDU_Type;
                   HeaderBlocks : in HeaderBlocks_Type ) is
  InOutFitsStreamAccess : Stream_Access := Stream(HDU.FitsFile);
  -- sizes in units of Blocks
  HeaderSize : Positive := HeaderBlocks'Length; --(( Header'Length - 1 )/CardsCntInBlock) + 1;
  FileSpace  : Natural  := HDU.Positions.Header_Size / BlockSize;
  -- Header_Size no need to check: is multiple of BlockSize because we read by BlockSize
  Cur_Index : Positive_Count := Index(HDU.FitsFile);-- raises exception if file not open
 begin

    -- if not initialized, initilaze it
    -- Only this whe initialzed: Update HDU (without truncation, Inout_Mode): Open     with Inout_Mode -> switch to Inout_Mode below for update-without-truncation
    if HDU.Positions = HDU_Null then
     -- 3 cases when Positions is left unitialized:
     -- Create HDU at begining of the file (Out_Mode): -> file will be truncuted and HDU created as first HDU
     -- Create HDU at the end of an existing file (Append_Mode) -> new HDU will be added to the end of the file
     -- Create/Open(HDU) not called -> rely on Create/Open File to raise exception File_Not_opened
     HDU.Positions := Parse_HDU_Positions ( HeaderBlocks, Cur_Index );
    end if;

    if HeaderSize /= FileSpace then
     null;
     -- raise excpetion Constraint error ?
    end if;

    Set_Mode(HDU.FitsFile,In_File);
    Set_Mode(HDU.FitsFile,Out_File);
    -- this gives write access _without_truncation_
    -- See GNAT manual:
    -- GNAT, The GNU Ada 95 Compiler
    -- GNAT Academic Edition, Version 2005
    -- Document revision level 1.439
    -- Date: 2005/04/22 09:59:43
    -- A special case occurs with Stream_IO. As shown in the above table,
    -- the file is initially opened in `r' or `w' mode for the In_File and
    -- Out_File cases. If a Set_Mode operation subsequently requires switching
    -- from reading to writing or vice-versa, then the file is reopened in `r+'
    -- mode to permit the required operation.<<
    Set_Index(HDU.FitsFile, Positive_Count(HDU.Positions.Header_Index));
    HeaderBlocks_Type'Write(InOutFitsStreamAccess,HeaderBlocks);
  end Write;

 procedure Write ( HDU    : in out HDU_Type;
                   Header : in Header_Type ) is
 begin
  Write(HDU, To_HeaderBlocks(Header));
 end Write;

 -- positioning

 -- note: [Ada-wikibook] since Ada2012 pragma Inline() obsolete

 -- retuned index will point to first element of a block
 function To_FileIndex( BlockIndex : Positive ) return Positive is
  F_Ix : Positive := (BlockIndex - 1) * BlockSize + 1;
 begin
  return F_Ix;
 end To_FileIndex;
 -- retuned index will point to a block which contains
 -- the element to which FileIndex points
 function To_BlockIndex( FileIndex : Positive ) return Positive is
  B_Ix : Positive := (FileIndex - 1) / BlockSize + 1;
 begin
  return B_Ix;
 end To_BlockIndex;
 -- FIXME FileIndex and Block Size must count in same units:
 -- FileIndex is now Stream_IO so Element_Size in GNAT is Byte
 function To_BlockSize( Size : Natural ) return Natural is
  SizeInBlocks : Natural := 0;
 begin
  if Size /= 0 then
   SizeInBlocks := (Size - 1) / BlockSize + 1;
  end if;
  return SizeInBlocks;
 end To_BlockSize;

 function Size( Header : Header_Type ) return Natural
 -- returns Header size in Blocks
 is
 begin
  if 0 = Header'Length then
   return 0;
  else
   return 1 + (Header'Length - 1) / CardsCntInBlock;
  end if;
 end Size;

  -- return index from start of the FITS file where Header and DataUnit start
 function Header_Index( HDU : HDU_Type ) return Positive
 is
 begin
  return To_BlockIndex(HDU.Positions.Header_Index);
 end Header_Index;

 function Data_Index  ( HDU : HDU_Type ) return Positive
 is
 begin
  return To_BlockIndex(HDU.Positions.Data_Index);
 end Data_Index;

 function Header_Size( HDU : HDU_Type ) return Natural
 is
 begin
  return To_BlockSize(HDU.Positions.Header_Size);
 end Header_Size;

 function Data_Size  ( HDU : HDU_Type ) return Natural
 is
 begin
  return To_BlockSize(HDU.Positions.Data_Size);
 end Data_Size;

 procedure Set_Index(HDU : HDU_Type; Index : Positive )
 is
 begin
   Set_Index(HDU.FitsFile,Positive_Count(To_FileIndex(Index)));
 end Set_Index;

 function Index(HDU : HDU_Type) return Positive
 is
 begin
   return To_BlockIndex(Positive(Index(HDU.FitsFile)));
 end Index;

 function  Size(HDU : HDU_Type) return Positive
 is
 begin
   return To_BlockSize( Natural(Size(HDU.FitsFile)) );
 end Size;

-- FIXME CopyBlocks is FITS_File_Type operation
-- this serves only as workaround until FITS_File_Type API implemented
 procedure Copy_Blocks( FromFile  : HDU_Type;
                        FromBlock : Positive; ToBlock : Natural;
                        ToFile  : HDU_Type )
 is
  NBlocks : Positive := 100;-- BufferSize multiplier
  subtype  BigBuffer_Type is String(1..BlockSize*NBlocks);
  BigBuffer : BigBuffer_Type;
  NBlocksToCopy : Natural; -- nothing to copy possible: ToBlocks < FromBlocks
  subtype  Buffer_Type is String(1..BlockSize);
  Buffer : Buffer_Type;
  BCnt : Positive := 1;
  TotBlocksToCopy : Natural; -- nothing to copy possible: ToBlocks < FromBlocks
  BlocksToCopy : Natural;
 begin

   Set_Index( FromFile, FromBlock );
   TotBlocksToCopy := 1 + ToBlock - FromBlock ;

  Ada.Text_IO.Put_Line("DBG TotBlocksToCopy > " & Positive'Image(TotBlocksToCopy) );

   NBlocksToCopy := TotBlocksToCopy / NBlocks;
   BlocksToCopy  := TotBlocksToCopy mod NBlocks;

   Ada.Text_IO.Put_Line("DBG NBlocksToCopy > " & Positive'Image(NBlocksToCopy) );
   Ada.Text_IO.Put_Line("DBG BlocksToCopy  > " & Positive'Image(BlocksToCopy) );

   loop
     exit when End_Of_File(FromFile.FitsFile)
            or (NBlocksToCopy <= 0);
     BigBuffer_Type'Read (Stream(FromFile.FitsFile), BigBuffer);
     BigBuffer_Type'Write(Stream(ToFile.FitsFile),   BigBuffer);
     NBlocksToCopy := NBlocksToCopy - 1;
   end loop;

   loop
     exit when End_Of_File(FromFile.FitsFile)
            or (BlocksToCopy <= 0);
     Buffer_Type'Read (Stream(FromFile.FitsFile), Buffer);
     Buffer_Type'Write(Stream(ToFile.FitsFile),   Buffer);
     BlocksToCopy := BlocksToCopy - 1;
   end loop;
   -- FIXME should loop-exit with EOF be considered an error?

-- Ada.Text_IO.Put_Line("DBG BCnt > " & Positive'Image(BCnt) );

 end;

end HDU;

