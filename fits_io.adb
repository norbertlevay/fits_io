--
-- FITS_IO implementation notes
--
-- Current implementation is based on two elements:
--
-- 1, Low-level FITS-file access
-- This presents a FITS-file as array of 2880-blocks.
-- The layer provides positioning by blocks, read and write.
-- Currently implemented with Stream_IO. Possible with Direct_IO which would
-- eliminate block position calculations.
--
-- 2, HDU information vector
-- This is a vector of structures which store HDU's positions and sizes
-- (and other 'useful' info). It is part of File-handle record (FIST_IO.File_Type)
-- and is filled in Open/Create and Write, and accessed in Read.
-- Currently it is implemented as static vector of MaxHDU records.
-- Should be implemented as dynamic vector of 3..5 record-chunks.

-- FIXME error handling/exceptions; not implemented yet

-- FIXME reconsider Open/Create + File_mode rules / add to doc:
-- valid combinations:
-- Open   : In Inout Append
-- Create : Append
-- Write behaviour:
-- Open   + Out_File    -> Write(...,HDU_Num) truncates FITS-File and appends Header to the truncated end
-- Open   + Inout_File  -> Write(...,HDU_Num) overwrites HDU if sizes match (sizes counted in Blocks = 2880bytes)
-- Open/Create + Append -> Write() (call without HDU_Num ) appends to the end


with Ada.Text_IO;-- for debug only
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;


package body FITS_IO is

 -- HDU positions within FITS-file

 type HDU_Pos is record
  HeaderStart : Positive; -- BlockIndex where header starts with the FitsFile
  HeaderSize  : Positive; -- Header size in Blocks (0 not allowed, HDU starts always with Header)
  DataStart   : Positive; -- BlockIndex where header starts with the FitsFile
  DataSize    : Natural;  -- Data size in Blocks (0 allowed, Primary HDU may have no data)
 end record;

 -- File_Type will hold to array of those above for each HDU in FITS-file

 MaxHDU : constant Positive := 10;
 type HDU_Data is record
  HDUPos  : HDU_Pos;
  HDUInfo : HDU_Info;
 end record;
 type HDU_Data_Array_Type is array (Positive range 1 .. MaxHDU) of HDU_Data;

 type File_Data is record
  FitsFile : Ada.Streams.Stream_IO.File_Type;
  Mode     : File_Mode;-- FITS_IO Mode
  HDU_Cnt  : Natural;
  HDU_Arr  : HDU_Data_Array_Type;
 end record;

 --

 -- FIXME consider to return only one preformatted string per HDU
 function List_HDUInfo ( File : File_Type ) return HDU_Info_Arr
 is
  All_HDU : HDU_Info_Arr( 1 .. File.HDU_Cnt ) := (others=>Null_HDU_Info);
 begin
  return All_HDU;
 end List_HDUInfo;


 procedure Copy_HDU (FromFile : File_type; FirstHDU : Positive; LastHDU : Positive;
                     ToFile   : File_type)
 is
 begin
  null;
 end;



---------------------------
-- Low-level file access --
---------------------------

 -- Read/Write implemented with Stream_IO

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


 --
 -- returns DU-size in Blocks
 --
 function  Calc_DataUnit_Size( AxesDimensions : in Axes_Type )
  return Natural
 is
  DUSize : Natural := 0;
 begin

   if AxesDimensions.Naxes > 0 then
     DUSize := 1;
     for I in 1..AxesDimensions.Naxes loop
      exit when AxesDimensions.Naxis(I) = 0;
      DUSize := DUSize * AxesDimensions.Naxis(I);
     end loop;
     if DUSize /= 0 then
      DUSize := DUSize * (abs AxesDimensions.BitPix/8);-- 8 bits in Octet
      DUSize := 1 + DUSize/BlockSize;
      -- size in Blocks
     end if;
   end if;

  return DUSize;
 end Calc_DataUnit_Size;

 --
 -- parse keywords needed to calculate DU size
 --
 procedure Parse_KeyRecord( Card : in Card_Type;
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
     -- TODO what to do if NAXIS and NAXISnn do not match in a broken FITS-file
     -- [FITS,Sect 4.4.1.1]: NAXISn keys _must_ match NAXIS keyword.

   end if;
 end Parse_KeyRecord;

 --
 -- walk through each HeaderBlock
 --
 function  Parse_HeaderBlock( Block : in Block_Type ;
                              CardCount : out Positive;
                              AxesDimensions : in out Axes_Type )
  return Boolean
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
  CardCount := CardCnt;
  return ENDFound;
 end Parse_HeaderBlock;

 -- [GNAT,9.8 Stream_IO] "The type Stream_Element is simply a byte."
 -- [Ada2005, 13.7 The Package 'System'] "Storage_Unit The number of bits per storage element."
 -- [GNAT,2 Implementation Defined Atrributes ->Bit] "...from System.Storage_Unit (=Byte)..."
 function  To_BlockIndex( OctetIndex : in  Positive ) return Positive is
  begin
   return (OctetIndex - 1) / BlockSize + 1;
  end To_BlockIndex;

 function  To_OctetIndex( BlockIndex : in  Positive ) return Positive is
  begin
   return (BlockIndex - 1) * BlockSize + 1;
  end To_OctetIndex;
 -- FIXME not implemented yet: Use System.Storage_Unit to implement the above
 -- FIXME not implemented yet: Endianess: System.Bit_Order : High_Order_First(=BigEndian) Low_Order_First Default_Bit_Order

 --
 -- for Open - using existing HDU's
 --
 procedure Parse_HDU_Positions ( File : in out File_Type )
 is
   FitsSA  : Ada.Streams.Stream_IO.Stream_Access :=
             Ada.Streams.Stream_IO.Stream( File.FitsFile );
   -- HDU_Arr : HDU_Data_Array_Type;

   -- controlling the loops
   HDU_Cnt : Positive := 1;
   EndCardFound : Boolean := false;

   -- positions & size in file
   HeadStart_Index : Positive;
   DataStart_Index : Positive;
   DataUnit_Size   : Natural := 0;

   AxesDimensions : Axes_Type;
   Block : BlockArray_Type(1..1);

   CurCardCnt : Natural;
   TotCardCnt : Natural := 0;
 begin

   while not Ada.Streams.Stream_IO.End_OF_File(File.FitsFile)
   loop

      AxesDimensions := Null_Axes;

      -- Header

      HeadStart_Index := Index( File );

      loop
         Block := Read( File );
         EndCardFound := Parse_HeaderBlock( Block(1) , CurCardCnt, AxesDimensions );
         TotCardCnt := TotCardCnt + CurCardCnt;
         exit when EndCardFound ;
      end loop;

      -- DataUnit

      DataStart_Index := Index(File);
      DataUnit_Size   := Calc_DataUnit_Size( AxesDimensions );
      Set_Index( File, DataStart_Index + DataUnit_Size );
      -- skip data unit
                                                              
      File.HDU_Arr(HDU_Cnt).HDUPos.HeaderStart := HeadStart_Index;
      File.HDU_Arr(HDU_Cnt).HDUPos.HeaderSize  := DataStart_Index - HeadStart_Index;
      File.HDU_Arr(HDU_Cnt).HDUPos.DataStart   := DataStart_Index;
      File.HDU_Arr(HDU_Cnt).HDUPos.DataSize    := DataUnit_Size;
      File.HDU_Cnt := HDU_Cnt;
      File.HDU_Arr(HDU_Cnt).HDUInfo.CardsCnt := TotCardCnt;
      -- store positions of this HDU

      HDU_Cnt := HDU_Cnt + 1;
      -- next HDU

   end loop;

  end Parse_HDU_Positions;

 --
 -- for Write/Append - creating new HDU from Header
 --
 -- Open/Create inits File.HDU_Arr <- sets correct File-Index
 -- Write takes and converts Header_Type --> HeaderBlockArray_Type
 -- writes the header into the file
 -- if write success calls this Parse_HDU_positions():
 function  Parse_HDU_Data( HeaderBlocks : in HeaderBlockArray_Type)
  return HDU_Data
 is
  HDUData : HDU_Data;
--  HDU_Cnt : Positive;
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

   HDUData.HDUPos.HeaderStart := 1;
   HDUData.HDUPos.HeaderSize  := HeaderBlocks'Length;
   HDUData.HDUPos.DataStart   := HDUData.HDUPos.HeaderStart
                               + HDUData.HDUPos.HeaderSize;
   HDUData.HDUPos.DataSize    := DU_Size;
   -- FIXME add HDUData.HDU_Info; not implemented yet

   return HDUData;
 end;

 function  Index ( File  : in File_Type ) return Positive
 is
 begin
  return To_BlockIndex(Positive(Ada.Streams.Stream_IO.Index( File.FitsFile )));
  -- FIXME verify this direct conversion Count -> Positive
 end Index;

 procedure Set_Index ( File  : in File_Type;
                       Index : in Positive ) -- Block Index
 is
 begin
  Ada.Streams.Stream_IO.Set_Index( File.FitsFile,
      Ada.Streams.Stream_IO.Positive_Count(To_OctetIndex(Index)) );
  -- FIXME verify this direct conversion Positive -> Positive_Count
 end Set_Index;

 procedure Write(File    : in File_Type;
                 Blocks  : in BlockArray_Type;
                 NBlocks : in Positive := 1)
 is
   FileSA : Ada.Streams.Stream_IO.Stream_Access :=
            Ada.Streams.Stream_IO.Stream(File.FitsFile);
 begin
   BlockArray_Type'Write( FileSA, Blocks );
 end Write;

 function  Read (File    : in  File_Type;
                 NBlocks : in  Positive := 1) return BlockArray_Type
 is
   FileSA : Ada.Streams.Stream_IO.Stream_Access :=
            Ada.Streams.Stream_IO.Stream(File.FitsFile);
   Blocks : BlockArray_Type( 1 .. NBlocks );
 begin
   BlockArray_Type'Read( FileSA, Blocks );
   return Blocks;
 end Read;

---------------
-- Interface --
---------------

 -- for debug only
 procedure Print_FitsFileHandle (Fits : in File_Type) is
  FileSize : Positive;
 begin
  Ada.Text_IO.Put_Line("HDU_Cnt : " & Natural'Image(Fits.HDU_Cnt));
--  for I in Fits.HDU_Arr'Range -- this shows also not used entries
  for I in 1..Fits.HDU_Cnt
  loop
   Ada.Text_IO.Put("HDU#" & Integer'Image(I) );
   Ada.Text_IO.Put(" H: " & Integer'Image(Fits.HDU_Arr(I).HDUPos.HeaderStart));
   Ada.Text_IO.Put(" (" & Integer'Image(Fits.HDU_Arr(I).HDUPos.HeaderSize) & ") ");

   Ada.Text_IO.Put(" DU: " & Integer'Image(Fits.HDU_Arr(I).HDUPos.DataStart));
   Ada.Text_IO.Put_Line(" (" & Integer'Image(Fits.HDU_Arr(I).HDUPos.DataSize) & ")");
  end loop;
  FileSize := (Fits.HDU_Arr(Fits.HDU_Cnt).HDUPos.DataStart - 1 + Fits.HDU_Arr(Fits.HDU_Cnt).HDUPos.DataSize)*BlockSize;
  Ada.Text_IO.Put_Line("LastDU LastByte (=FileSize): " & Integer'Image(FileSize));
 end;

 -- FIXME File_Mode conversions : check this out; is there a better way ?
 function To_StreamFile_Mode ( Mode : File_Mode )
  return Ada.Streams.Stream_IO.File_Mode
 is
  StreamMode : Ada.Streams.Stream_IO.File_Mode;
 begin
  case Mode is
   when In_File =>
    StreamMode := Ada.Streams.Stream_IO.In_File;
   when Out_File =>
    StreamMode := Ada.Streams.Stream_IO.Out_File;
   when Inout_File =>
    StreamMode := Ada.Streams.Stream_IO.Out_File;-- Stream_IO : switch mode will result in Inout access
   when Append_File =>
    StreamMode := Ada.Streams.Stream_IO.Append_File;
  end case;
  return StreamMode;
 end To_StreamFile_Mode;


 procedure Open ( Fits : in out File_Type;
                  Mode : in File_Mode;
                  Name : in String;
                  Form : in String   := "shared=no")
 is
 begin
  Ada.Text_IO.Put_Line("DBG Open: "&Name);

  Fits := new File_Data;

  Ada.Streams.Stream_IO.Open( Fits.FitsFile,
                Ada.Streams.Stream_IO.In_File,
                Name,Form);--[GNAT,9.2 FORM strings]

  Parse_HDU_Positions ( Fits ); -- Fills in HDU data to File
  Print_FitsFileHandle(Fits);-- debug

  Ada.Streams.Stream_IO.Set_Mode(Fits.FitsFile,To_StreamFile_Mode(Mode));
  Fits.Mode := Mode;
 end Open;



 procedure Close ( Fits : in out File_Type ) is
  procedure Delete_FileType is new Ada.Unchecked_Deallocation
                                            (File_Data, File_Type);
 begin
  Ada.Streams.Stream_IO.Close(Fits.FitsFile);
  Fits.HDU_Cnt := 0;-- not needed
  Delete_FileType(Fits);
 end Close;

 procedure Set_Mode ( File : in out File_Type;
                      Mode : in File_Mode ) is
 begin
  File.Mode := Mode;
  Ada.Streams.Stream_IO.Set_Mode(File.FitsFile,To_StreamFile_Mode(File.Mode));
 end Set_Mode;

 function Mode ( File : in  File_Type )
  return File_Mode is
 begin
  return File.Mode;
 end Mode;


 -- FIXME review these record definitions & their usage
 function To_BlockArray(HeaderBlocks : HeaderBlockArray_Type )
  return BlockArray_Type
  is
    BA : BlockArray_Type(HeaderBlocks'Range);
    first, last : Positive;
  begin
   for I in HeaderBlocks'Range loop     -- N blocks of
    for J in HeaderBlocks(I)'Range loop -- 36 cards
      first := (J-1)*CardSize + 1;
      last  := first + CardSize - 1;
      BA(I)(first..last) := HeaderBlocks(I)(J)(1..CardSize); -- copy one card
    end loop;
   end loop;
   return BA;
  end To_BlockArray;

 -- behaviour depends on Mode in Open/Create
 -- Inout_Mode updates the HDU given by HDU_Num but only if the size (in blocks) match
 -- Out_Mode will truncate file at HDU_Num and then append the Header
 -- Append_Mode (called without HDU_Num) appends the Header to the end of the file
 -- Note:
 -- FIXME HDU_AfterLast is tight to Positive range definition of HDU_Arr
 -- FIXME is this goos idea at all -> Better use separate Append(File,Header) besides Write(File,Header,HDU_Num)
 procedure Write ( File    : in  File_Type;
                   Header  : in  Header_Type;
                   HDU_Num : in  Positive := HDU_AfterLast )-- default: Append
 is
  HeaderBlocks : HeaderBlockArray_Type := To_HeaderBlocks(Header);
  HDUData      : HDU_Data  := Parse_HDU_Data( HeaderBlocks );
  CurMode      : File_Mode := Mode(File);
  HDUIx,FileIx : Positive;
  -- Fits File consists of sequence of HDU's with
  --   different size numbered by HDU-index: 1,2.3...
  -- File.HDU_Arr connects FileIx and HDUIx where:
  --   FileIx is position of the HDU in file given blocks
  --   HDUIx  is the HDU-index itself
 begin

  -- 0. Sanity checks on HDU_Num

  if (File.HDU_Cnt = 0) and
     (HDU_Num /= 1)
  then
   null; -- raise exception and exit... UserMsg: to empty file we can write only 1st HDU
  end if;

  -- Note: user cannot supply HDU_Num HDU_Cnt+1 in attempt to do append.
  -- For append Append_Mode must be used. FIXME re-consider this (?).
  if (HDU_Num > File.HDU_Cnt) and
     (HDU_Num /= HDU_AfterLast)
  then
   null; -- raise exception and exit... UserMsg: HDU_Num out of range for given file & mode combination
  end if;

  -- for Inout_Mode (e.g. file update) check sizes:
  -- space in File vs Header+Data as defined be the new Header
  -- FIXME note: if User wants update last HDU and sizes differ should we allow ?
  if CurMode = Inout_File
  then
    null; -- FIXME check sizes for Inout-write ; not implemented yet
    -- raise exception if sizes don't match UserMsg: For updating HDU (Inout mode) HDU sizes must match.
  end if;

  -- 1. Set HDUIx and FileIx depending on the situation

  if File.HDU_Cnt = 0
  then
   -- writing to an empty file
   HDUIx  := 1;
   FileIx := 1;
  else
   -- updating existing (non-empty) file
   case CurMode is
     when Inout_File|Out_File =>
       HDUIx  := HDU_Num;
       FileIx := File.HDU_Arr(HDU_Num).HDUPos.HeaderStart;
     when Append_File =>
       HDUIx  := File.HDU_Cnt + 1;
       FileIx := File.HDU_Arr(File.HDU_Cnt).HDUPos.DataStart
               + File.HDU_Arr(File.HDU_Cnt).HDUPos.DataSize;
     when others =>
       null;-- FIXME raise exception or let Stream_IO.Write raise exception?
       Ada.Text_IO.Put_Line("ERROR case CurMode undef " & File_Mode'Image(CurMode));
   end case;
  end if;

  -- 2, Write data to file
  Set_Index(File,FileIx);
  Write(File, To_BlockArray(HeaderBlocks));
  -- FIXME define convertible Header <-> Block arrays is possible ?

  -- 3, Update File.HDU_Arr if write successful...
  File.HDU_Arr(HDUIx).HDUPos.HeaderStart := FileIx; -- FIXME what does Parse_HDU_Data() do?
  File.HDU_Arr(HDUIx).HDUPos.HeaderSize  := HDUData.HDUPos.HeaderSize;
  File.HDU_Arr(HDUIx).HDUPos.DataStart   := FileIx + HDUData.HDUPos.HeaderSize;
  File.HDU_Arr(HDUIx).HDUPos.DataSize    := HDUData.HDUPos.DataSize;
  -- HDUIx is now last HDU, except in Inout case
  if CurMode /= Inout_File then
   File.HDU_Cnt := HDUIx;
  end if;

 end Write;

 -- Create FitsFile
 -- if Mode Out_File   : write will create first HDU of a new file
 -- if Mode Append_Mode: write will create new HDU at the end of a file
 -- FIXME check Create + Append behaviour: 2nd case should map to OpenFile in Append_Mode ?
 procedure Create ( Fits : in out File_Type;
                    Mode : in File_Mode;
                    Name : in String;
                    Form : in String    := "shared=no") is
 begin
  Ada.Text_IO.Put_Line("DBG Create: "&Name);
  Fits := new File_Data;
  Ada.Streams.Stream_IO.Create( Fits.FitsFile,
                To_StreamFile_Mode(Mode),
                Name,Form);
  Fits.HDU_Cnt := 0;-- init HDU_Arr
  Ada.Streams.Stream_IO.Set_Mode(Fits.FitsFile, To_StreamFile_Mode(Mode));
  Fits.Mode    := Mode;
 end Create;

 function CardsCount (HeaderBlocks : HeaderBlockArray_Type) return Positive
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
 -- FIXME we need CardsCount stored in the HeaderBlockArray_Type

 -- strip empty spaces from start end end of each line
 -- strip empty cards after END-card
 function To_Header( HeaderBlocks : HeaderBlockArray_Type ) return Header_Type
 is
  H  : Header_Type(1 .. CardsCount(HeaderBlocks));
  Ix : Positive := 1;
 begin
  for I in HeaderBlocks'Range loop
   for J in HeaderBlocks(I)'Range loop
    H(Ix) := Card.Trim(Card.To_Bounded_String(HeaderBlocks(I)(J)) , Ada.Strings.Both);
    exit when HeaderBlocks(I)(J) = ENDCard;
    Ix := Ix + 1;
   end loop;
  end loop;
  return H;
 end To_Header;

 function To_HeaderBlockType is
    new Ada.Unchecked_Conversion (Block_Type, HeaderBlock_Type);

 function To_BlockType is
    new Ada.Unchecked_Conversion (HeaderBlock_Type, Block_Type);

 function To_HeaderBlockArray(Blocks : BlockArray_Type)
  return HeaderBlockArray_Type
 is
  HBlocks : HeaderBlockArray_Type(Blocks'Range);
 begin
  for I in Blocks'Range loop
    HBlocks(I) := To_HeaderBlockType(Blocks(I));
  end loop;
  return HBlocks;
 end To_HeaderBlockArray;

 -- subtype Block_Type is String (1 .. BlockSize );
 -- type BlockArray_Type is array (Positive range <>) of Block_Type;
 -- type HeaderBlock_Type is array (1 .. CardsCntInBlock) of String(1..CardSize);
 -- type HeaderBlockArray_Type is array (Positive range <>) of HeaderBlock_Type;
 -- FIXME review Block, HeaderBlock, DataBlock type definitions,
 -- require too many unnecessary conversions (also see HDU.Read)
 -- Probably only Header <-> HeaderBlock is justified

 --
 -- Read header from FITS-file
 --
 function Read  ( File    : in  File_Type;
                  HDU_Num : in  Positive ) return Header_Type
 is
  Header : Header_Type(1..File.HDU_Arr(HDU_Num).HDUInfo.CardsCnt);
 begin
  -- sanity check (like in Write)
  if (HDU_Num > File.HDU_Cnt) and
     (HDU_Num /= HDU_AfterLast)
  then
   null; -- raise exception and exit... UserMsg: HDU_Num out of range for given file & mode combination
  end if;
  -- position by HDU_Num in file
  -- FIXME consider API Set_HDUIndex() -> and Read() Write() without HDU_Num
  Set_Index(File, File.HDU_Arr(HDU_Num).HDUPos.HeaderStart);
  -- read and convert to Header
  declare
    Blocks : BlockArray_Type := Read (File, File.HDU_Arr(HDU_Num).HDUPos.HeaderSize);
    HeaderBlocks : HeaderBlockArray_Type := To_HeaderBlockArray(Blocks);
  begin
    Header := To_Header(HeaderBlocks);
  end;
  return Header;
 end Read;

 --
 --
 --
 function To_HeaderBlocks( Header : Header_Type ) return HeaderBlockArray_Type
 is
  -- init blocks with space-characters
  HB : HeaderBlockArray_Type(1 .. (1 + (Header'Length -1) / CardsCntInBlock)) := (others => EmptyBlock);
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
     str : String := Card.To_String(Card.Trim(Header(I), Ada.Strings.Both));
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


end FITS_IO;
