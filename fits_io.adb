--
-- FITS_IO implementation notes
--
-- Current implementation is based on two elements:
--
-- 1, Low-level FITS-file access
-- This presents a FITS-file as array of blocks, each block having 2880 octets.
-- The layer provides positioning by blocks, read and write.
-- Currently implemented with Stream_IO.
--
-- Possible with Direct_IO which would eliminate block position calculations.
-- However Direct_IO does not allow to write more blocks with one write so needs cycle,
-- and Read is procedure not a function, then cannot initialize unconstrained arrays,
-- and so also needs cycle by Block.
-- Decided in favour of Stream_IO.
--
-- 2, FITS-File HDU-lists
-- This is a vector of structures which store HDU's positions and sizes
-- (and other 'useful' info). It is part of File-handle record (FIST_IO.File_Type)
-- and is filled in Open/Create and Write, and accessed in Read.


-- FIXME File_Type is in IO-pkgs derived from FileControlBlock:
-- type Direct_AFCB is new FCB.AFCB with record
--  ... new data fields if needed ...
-- end record;
-- Then it inherits all operations on File_Type

-- HDU_Num is used ony as selector/index in array.
-- It does not participate in offset/Index calculations.
-- Can have type of his own & these dependent on it:
-- File.HDU_Cnt & HDU_AfterLast
-- FIXME HDU_AfterLast is tight to Positive range definition of HDU_Arr
-- Note: [FITS 3.1 Overall File organization]:
-- "This standard does not impose a limit on the total size of a
-- FITS file, nor on the size of an individual HDU within a FITS file."

-- FIXME error handling/exceptions; not implemented yet

with Ada.Text_IO;-- for debug only
with Ada.Streams.Stream_IO; --use Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with Ada.Containers.Vectors; -- since Ada 2005

package body FITS_IO is

 ---------------------------------------------------
 -- Low level FITS-Headers access by HeaderBlocks --
 ---------------------------------------------------

 package SIO renames Ada.Streams.Stream_IO;

 -- NOTE Count defs moved to ads for Data access Read/Write params
-- type Count is new SIO.Count;
 -- will inherit Index() Set_Index() from SIO for Fits_IO.Positive_Count
 -- OR like Ada.Direct_IO  - System.Direct_IO:
 -- type Count is range 0 .. Ada.Streams.Stream_IO.Count'Last;
 -- but this will not inherit Index Set_Index calls and conversion needed
-- subtype Positive_Count is Count range 1 .. Count'Last;
 -- FIXME we use Count whenever zero value is needed - consider introduce Natural_Count ?
 -- Why not done in standard *_IO packages? computaions may result in negative numbers?

 BlockSize       : constant Positive_Count := 2880; -- [FITS, Sect 3.1]
 CardsCntInBlock : constant Positive_Count := BlockSize / Positive_Count(CardSize);
 -- [FITS 3.3.1 Primary Header] 36 cards per block        -- conv ok, CardSize 80 const

 subtype Card_Type is String(1..CardSize);
 -- makes sure index start with 1

 type HeaderBlock_Type is array (1 .. CardsCntInBlock) of Card_Type;
 type HeaderBlockArray_Type is array (Positive_Count range <>) of HeaderBlock_Type;
   -- Header format inside FITS-file

 ENDCard    : constant Card_Type := "END                                                                             ";
 EmptyCard  : constant Card_Type := (others => ' ');
 EmptyBlock : constant HeaderBlock_Type := (others => EmptyCard);

 function  To_SIO is new Ada.Unchecked_Conversion (File_Mode, SIO.File_Mode);
 -- function  To_FITS_IO is new Ada.Unchecked_Conversion (SIO.File_Mode, File_Mode);
 -- use type FCB.File_Mode; FIXME ?? see -> a-ststio.adb

 -- positioning in file

 function  To_BlockIndex( OctetIndex : in  Positive_Count ) return Positive_Count is
  begin
   return (OctetIndex - 1) / BlockSize + 1;
  end To_BlockIndex;

 function  To_OctetIndex( BlockIndex : in  Positive_Count ) return Positive_Count is
  begin
   return (BlockIndex - 1) * BlockSize + 1;
  end To_OctetIndex;

 pragma Inline (To_BlockIndex);
 pragma Inline (To_OctetIndex);
 -- Note: since Ada2012 pragma Inline obsolete, add anyway for older compilers

 -- Fits_IO.Count inhereted from SIO.Count:
 -- Index() and Set_Index() are inherited from SIO.[Set_]Index()
 function  BlockIndex ( File  : in SIO.File_Type ) return Positive_Count
 is
 begin
  return To_BlockIndex(Index( File ));
 end BlockIndex;

 procedure Set_BlockIndex ( File  : in SIO.File_Type;
                            Index : in Positive_Count ) -- Block Index
 is
 begin
  Set_Index( File, To_OctetIndex(Index) );
 end Set_BlockIndex;

 pragma Inline (BlockIndex);
 pragma Inline (Set_BlockIndex);

 --
 -- Header access
 --
 procedure Write(File    : in SIO.File_Type;
                 Blocks  : in HeaderBlockArray_Type)
 is
 begin
   HeaderBlockArray_Type'Write( SIO.Stream(File), Blocks );
 end Write;

 function  Read (File    : in  SIO.File_Type;
                 NBlocks : in  Positive_Count := 1) return HeaderBlockArray_Type
 is
   Blocks : HeaderBlockArray_Type( 1 .. NBlocks );
 begin
   HeaderBlockArray_Type'Read( SIO.Stream(File), Blocks );
   return Blocks;
 end Read;


  -------------------------
  -- FITS-File HDU-lists --
  -------------------------

 -- HDU Records
 -- It is linked list of HDU info about
 -- positions and sizes of HDU's in FITS File.
 -- Open and Write will insert HDU Records into the list
 -- Create initializes an empty list
 -- Close destroys the list
 --
 -- FIXME: Currently it is implemented as static vector of MaxHDU records.
 -- Should be implemented as dynamic vector of 3..5 record-chunks, or a list.

 type HDU_Pos_Type is record
  HeaderStart : Positive_Count; -- BlockIndex where header starts with the FitsFile
  HeaderSize  : Positive_Count; -- Header size in Blocks (0 not allowed, HDU starts always with Header)
  DataStart   : Positive_Count; -- BlockIndex where header starts with the FitsFile
  DataSize    : Count;    -- Data size in Blocks (0 allowed, Primary HDU may have no data)
 end record;

 type HDU_Data_Type is record
  HDUPos  : HDU_Pos_Type;
  HDUInfo : HDU_Info_Type; -- part of external API (.ads)
 end record;

 MaxHDU : constant Positive := 10;
 type HDU_Data_Array_Type is array (Positive range 1 .. MaxHDU) of HDU_Data_Type;

 type File_Type_Record is record
  BlocksFile : SIO.File_Type;
  Mode       : File_Mode;-- FITS_IO.Mode
  HDU_Cnt  : Natural; -- After Create there is no HDU's (=0) and needed because HDU_Arr is static vector so HDU_Arr'Length /= HDU_Cnt
  HDU_Arr  : HDU_Data_Array_Type;
 end record;

 -- for debug only
 procedure Print_FitsFileHandle (Fits : in File_Type) is
  FileSize : Positive_Count;
 begin
  Ada.Text_IO.Put_Line("HDU_Cnt : " & Natural'Image(Fits.HDU_Cnt));
--  for I in Fits.HDU_Arr'Range -- this shows also not used entries
  for I in 1..Fits.HDU_Cnt
  loop
   Ada.Text_IO.Put("HDU#" & Natural'Image(I) );
   Ada.Text_IO.Put(" H: " & Positive_Count'Image(Fits.HDU_Arr(I).HDUPos.HeaderStart));
   Ada.Text_IO.Put(" ("   & Count'Image(Fits.HDU_Arr(I).HDUPos.HeaderSize) & ") ");

   Ada.Text_IO.Put(" DU: "   & Positive_Count'Image(Fits.HDU_Arr(I).HDUPos.DataStart));
   Ada.Text_IO.Put_Line(" (" & Count'Image(Fits.HDU_Arr(I).HDUPos.DataSize) & ")");
  end loop;
  FileSize := (Fits.HDU_Arr(Fits.HDU_Cnt).HDUPos.DataStart - 1 + Fits.HDU_Arr(Fits.HDU_Cnt).HDUPos.DataSize)*BlockSize;
  Ada.Text_IO.Put_Line("LastDU LastByte (=FileSize): " & Count'Image(FileSize));
 end;

 ---------------------------------------
 -- start HDU vector/list implementation

 -- search for: HDUV and HDUVect
 -- implemented in:
 -- * Parse_HDU_Positions

 package HDUV is new Ada.Containers.Vectors
 	( Element_Type => HDU_Data_Type,
 	  Index_Type   => Positive);

 HDUVect : HDUV.Vector;

 procedure HDUVect_debug ( v : in HDUV.Vector) is
  lhdu : HDU_Data_Type;
  procedure printout( c : HDUV.Cursor) is
  begin
    Ada.Text_IO.Put_Line("HDUVect Cards: " & Integer'Image(HDUV.Element(c).HDUInfo.CardsCnt));
  end;
 begin

   Ada.Text_IO.Put_Line("HDUVect Length / Capacity: " & Ada.Containers.Count_Type'Image(HDUV.Length(v)) &
                                                " / " & Ada.Containers.Count_Type'Image(HDUV.Capacity(v)));
   HDUV.Iterate(v,printout'Access);

   -- example access by HDU_Num
   if Integer(HDUV.Length(v)) >= 1 then -- FIXME Integer() cast
    lhdu := HDUV.Element(v,1);
    Ada.Text_IO.Put_Line("HDUVect lhdu[1].HDUInfo.CardsCnt  : " & Integer'Image(lhdu.HDUInfo.CardsCnt));
   end if;

 end;

 -- end   HDU vector/list implementation
 ---------------------------------------

 --------------------------------------------------
 -- Header definition as stored inside FITS-file --
 --------------------------------------------------

 --
 -- returns DU-size in Blocks [FITS 4.4.1.1 Primary Header,(1)]
 --
 function  Calc_DataUnit_Size( HDUInfo : in HDU_Info_Type )
  return Count
 is
  DUSize : Count := 0;
  BITPIXSize : Positive_Count := Positive_Count(abs (HDUInfo.BitPix)/8);
   -- conv ok, abs(BitPix) is always positive and max 64
 begin

   if HDUInfo.Naxes > 0 then
     DUSize := 1;
     for I in 1..HDUInfo.Naxes loop
      exit when HDUInfo.Naxis(I) = 0;
      DUSize := DUSize * HDUInfo.Naxis(I);
     end loop;
     if DUSize /= 0 then
      DUSize := DUSize * BITPIXSize;
      DUSize := 1 + DUSize/BlockSize;
     end if;
   end if;

  return DUSize;
 end Calc_DataUnit_Size;

 function  To_BITPIXType( BitPix : in Integer )
  return BITPIX_Type
 is
  bp : BITPIX_Type;
 begin
  case BitPix is
  when   8 => bp := Int8;
  when  16 => bp := Int16;
  when  32 => bp := Int32;
  when  64 => bp := Int64;
  when -32 => bp := Float32;
  when -64 => bp := Float64;
  when others =>
   null;
   -- FIXME raise exception "out of range"
  end case;
  return bp;
 end To_BITPIXType;

 --
 -- parse keywords needed to calculate DU size
 --
 procedure Parse_KeyRecord( Card : in Card_Type;
                            HDUInfo  : in out HDU_Info_Type )
 is
  dim : Positive;
 begin
   -- [FITS 4.1.2 Components]:
   -- pos 9..10 is '= '
   -- pos 31 is comment ' /'
   -- then : pos 10..20 is value
   if    (Card(1..9) = "BITPIX  =") then
     HDUInfo.Data   := To_BITPIXType(Integer'Value(Card(10..30)));
     HDUInfo.BitPix := abs Integer'Value(Card(10..30));
   elsif (Card(1..5) = "NAXIS") then

     if (Card(1..9) = "NAXIS   =") then
         HDUInfo.Naxes := Positive'Value(Card(10..30));
     else
         dim := Positive'Value(Card(6..8));
         HDUInfo.Naxis(dim) := Count'Value(Card(10..30));
     end if;
     -- TODO what to do if NAXIS and NAXISnn do not match in a broken FITS-file
     -- [FITS,Sect 4.4.1.1]: NAXISn keys _must_ match NAXIS keyword.

   end if;
 end Parse_KeyRecord;

 --
 -- walk through each HeaderBlock
 --
 function  Parse_HeaderBlock( Block : in HeaderBlock_Type ;
                              CardCount : out Positive;
                              HDUInfo : in out HDU_Info_Type )
  return Boolean
 is
  ENDFound : Boolean := False;
  Card     : Card_Type;
  CardCnt  : Positive := 1;
  TotCardsInBlock : Positive := Positive(CardsCntInBlock);
   -- Positive() conv ok, it is const 36
 begin

  for I in Block'Range loop
    Card := Block( I );
    Parse_KeyRecord( Card, HDUInfo );
    ENDFound := (Card = ENDCard);
    exit when ENDFound or CardCnt >= TotCardsInBlock;
    CardCnt := CardCnt + 1;
  end loop;
  CardCount := CardCnt;
  return ENDFound;
 end Parse_HeaderBlock;

 --
 -- from parsed HDU-info calc HDU positions
 --
 function  HDU_Info2Pos (HDUStart_BlockIndex : in Positive_Count; HDUInfo : in HDU_Info_Type )
  return HDU_Pos_Type
 is
  HDUPos : HDU_Pos_Type;
  DataUnit_Size : Count := Calc_DataUnit_Size( HDUInfo );
  -- from parsed [BITPIX,NAXIS,NAXISi] keywords calc DataUnit size
 begin

  HDUPos.HeaderStart := HDUStart_BlockIndex;
  HDUPos.HeaderSize  := 1 + Positive_Count(HDUInfo.CardsCnt) / CardsCntInBlock;
   -- FIXME conv Positive_Count: problem if Positive'Last > Positive_Count'Last
   -- problem somewhat artificial: in practice CardsCount << then any of the above.
   -- nevertheless there is no rule -> how to deal with such situations?
  HDUPos.DataStart   := HDUPos.HeaderStart + HDUPos.HeaderSize;
  HDUPos.DataSize    := DataUnit_Size;
  -- all HDU start indexes and size in Blocks

  return HDUPos;
 end HDU_Info2Pos;

 --
 -- for Open - using existing HDU's
 --
 procedure Parse_HDU_Positions ( File : in out File_Type )
 is
   -- controlling the loops
   HDU_Cnt : Positive := 1;
   EndCardFound : Boolean := false;

   -- positions & size in file
   HeadStart_Index : Positive_Count;
   Next_HDU_Index  : Positive_Count;

   HDUInfo : HDU_Info_Type;
   Block : HeaderBlockArray_Type(1..1);

   CurCardCnt : Positive; -- written by Parse_HeaderBlock represents card-slots read/parsed
   TotCardCnt : Natural := 0;

   locHDUV : HDU_DATA_Type;-- for experimental HDUVect implementation
 begin

   -- HDUVectors variant
   HDUV.Reserve_Capacity(HDUVect,3); -- vector chunks of size 3
   HDUVect_debug(HDUVect);

   while not SIO.End_OF_File(File.BlocksFile)
   loop

      HDUInfo := Null_HDU_Info;
      TotCardCnt := 0;

      -- Header

      HeadStart_Index := BlockIndex( File.BlocksFile );

      loop
         Block        := Read( File.BlocksFile );
          -- raises exception when EOF reached, if not FITS-file
          -- FIXME ? it is ok, but might waste long time parsing big non-FITS file
         EndCardFound := Parse_HeaderBlock( Block(1) , CurCardCnt, HDUInfo );
         TotCardCnt   := TotCardCnt + CurCardCnt;
         exit when EndCardFound ;
      end loop;

      -- DataUnit

      HDUInfo.CardsCnt := TotCardCnt;-- FIXME Natural to Positive conversion

      -- HDU info cllected now store it to File_Type

      File.HDU_Arr(HDU_Cnt).HDUInfo := HDUInfo;
      File.HDU_Arr(HDU_Cnt).HDUPos  := HDU_Info2Pos(HeadStart_Index, HDUInfo);
      File.HDU_Cnt := HDU_Cnt;

      -- HDUVectors variant
      locHDUV.HDUInfo := HDUInfo;
      locHDUV.HDUPos  := HDU_Info2Pos(HeadStart_Index, HDUInfo);
      HDUV.Append(HDUVect, locHDUV);

      -- skip data unit for next HDU-read

      Next_HDU_Index := File.HDU_Arr(HDU_Cnt).HDUPos.DataStart + File.HDU_Arr(HDU_Cnt).HDUPos.DataSize;
      Set_BlockIndex( File.BlocksFile, Next_HDU_Index );

      HDU_Cnt := HDU_Cnt + 1;
      -- next HDU

   end loop;

   HDUVect_debug(HDUVect);

  -- Print_FitsFileHandle(File);-- debug

  end Parse_HDU_Positions;

 --
 -- for Write/Append - creating new HDU from Header
 --
 -- Open/Create inits File.HDU_Arr <- sets correct File-Index
 -- Write takes and converts Header_Type --> HeaderBlockArray_Type
 -- writes the header into the file
 -- if write success calls this Parse_HDU_Info & HDU_Info2Pos
 function  Parse_HDU_Info( HeaderBlocks : in HeaderBlockArray_Type)
  return HDU_Info_Type
 is
  HDUInfo  : HDU_Info_Type;
  ENDFound : Boolean := False;
  CurCardCnt : Positive; -- written by Parse_HeaderBlock represents card-slots read/parsed
  TotCardCnt : Natural := 0;
 begin

   for I in HeaderBlocks'Range
    loop
     EndFound   := Parse_HeaderBlock( HeaderBlocks(I) , CurCardCnt, HDUInfo );
     TotCardCnt := TotCardCnt + CurCardCnt;
     exit when ENDFound;
   end loop;

   HDUInfo.CardsCnt := TotCardCnt;

   return HDUInfo;
 end Parse_HDU_Info;

 ---------------
 -- Interface --
 ---------------

 procedure Open ( File : in out File_Type;
                  Mode : in File_Mode;
                  Name : in String;
                  Form : in String   := "shared=no")
 is
 begin
  File := new File_Type_Record;
  SIO.Open( File.BlocksFile,
                SIO.In_File,
                Name,Form);--[GNAT,9.2 FORM strings]
  Parse_HDU_Positions ( File ); -- Fills in HDU data to File
  SIO.Set_Mode(File.BlocksFile,To_SIO(Mode));
  File.Mode := Mode;
 end Open;

 --
 --
 --
 procedure Close ( File : in out File_Type ) is
  procedure Delete_FileType is new Ada.Unchecked_Deallocation
                                            (File_Type_Record, File_Type);
 begin
  SIO.Close(File.BlocksFile);
  Delete_FileType(File);
 end Close;

 --
 -- convert external Header represenation to FITS-internal format:
 -- * add space so that each card is 80 chars long
 -- * make sure space after END card is cleaned
 --
 function  To_HeaderBlockArray( Header : Header_Type ) return HeaderBlockArray_Type
 is
  -- init blocks with space-characters
  HB : HeaderBlockArray_Type(1 .. (1 + (Header'Length -1) / CardsCntInBlock)) := (others => EmptyBlock);
  Ix : Positive_Count := 1; -- index in header
  BIx, Cix : Count; -- block-index, card-index
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
  return HB;
 end To_HeaderBlockArray;

 -- behaviour depends on Mode in Open/Create
 -- Inout_Mode updates the HDU given by HDU_Num but only if the size (in blocks) match
 -- Out_Mode will truncate file at HDU_Num and then append the Header
 -- Append_Mode (called without HDU_Num) appends the Header to the end of the file
 -- Note:
 -- FIXME is this goos idea at all -> Better use separate Append(File,Header) besides Write(File,Header,HDU_Num)
 procedure Write ( File    : in out File_Type;
                   Header  : in Header_Type;
                   HDU_Num : in Positive := HDU_AfterLast )-- default: Append
 is
  HeaderBlocks : HeaderBlockArray_Type := To_HeaderBlockArray(Header);
  HDUInfo      : HDU_Info_Type := Parse_HDU_Info( HeaderBlocks );
  CurMode      : File_Mode := File.Mode;
  HDUIx  : Positive;
  FileIx : Positive_Count;
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
    if File.HDU_Arr(File.HDU_Cnt).HDUPos.HeaderSize /= (Header'Length / CardsCntInBlock + 1)
    then
     null;
     -- FIXME raise exception if sizes don't match
     -- UserMsg: For updating HDU (Inout mode) HDU sizes must match.
    end if;
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
       HDUIx  := File.HDU_Cnt + 1;-- FIXME check new HDU_Cnt is not pointing out of array-limit
       FileIx := File.HDU_Arr(File.HDU_Cnt).HDUPos.DataStart
               + File.HDU_Arr(File.HDU_Cnt).HDUPos.DataSize;
     when others =>
       null;-- FIXME raise exception or let Stream_IO.Write raise exception?
       Ada.Text_IO.Put_Line("ERROR case CurMode undef " & File_Mode'Image(CurMode));
   end case;
  end if;

  -- 2, Write data to file
  Set_BlockIndex(File.BlocksFile,FileIx);
  Write(File.BlocksFile, HeaderBlocks);

  -- 3, Update File.HDU_Arr if write successful...
  File.HDU_Arr(HDUIx).HDUPos := HDU_Info2Pos(FileIx,HDUInfo);

  -- HDUIx is now last HDU, except in Inout case
  if CurMode /= Inout_File then
   File.HDU_Cnt := HDUIx;
  end if;

 end Write;

 -- Create FitsFile
 -- if Mode Out_File   : write will create first HDU of a new file
 -- if Mode Append_Mode: write will create new HDU at the end of a file
 -- FIXME check Create + Append behaviour: 2nd case should map to OpenFile in Append_Mode ?
 procedure Create ( File : in out File_Type;
                    Mode : in File_Mode;
                    Name : in String;
                    Form : in String    := "shared=no") is
 begin
  File := new File_Type_Record;
  SIO.Create( File.BlocksFile,
                To_SIO(Mode),
                Name,Form);
  File.HDU_Cnt := 0; -- init HDU_Arr
  SIO.Set_Mode(File.BlocksFile,To_SIO(Mode));
  File.Mode := Mode;
 end Create;

 function  CardsCount (HeaderBlocks : HeaderBlockArray_Type) return Positive
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


 --
 -- convert internal Header format to external Header representation
 -- * strip empty spaces from start end end of each line
 -- * strip empty cards after END-card
 --
 function  To_Header( HeaderBlocks : HeaderBlockArray_Type ) return Header_Type
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

 --
 -- Read header from FITS-file
 --
 function  Read ( File    : in  File_Type;
                  HDU_Num : in  Positive ) return Header_Type
 is
  Header  : Header_Type(1..File.HDU_Arr(HDU_Num).HDUInfo.CardsCnt);
 begin

  -- sanity check (like in Write)
  if (HDU_Num > File.HDU_Cnt) and
     (HDU_Num /= HDU_AfterLast)
  then
   null; -- raise exception and exit... UserMsg: HDU_Num out of range for given file & mode combination
  end if;

  -- position by HDU_Num in file
  -- FIXME consider API Set_HDUIndex() -> and Read() Write() without HDU_Num
  Set_BlockIndex(File.BlocksFile, File.HDU_Arr(HDU_Num).HDUPos.HeaderStart);

  -- read and convert to Header
  declare
    HeaderBlocks : HeaderBlockArray_Type := Read (File.BlocksFile, File.HDU_Arr(HDU_Num).HDUPos.HeaderSize);
  begin
    Header := To_Header(HeaderBlocks);
  end;

  return Header;
 end Read;

 --
 --
 --
 function  List_HDUInfo ( File : File_Type ) return HDU_Info_Arr
 is
  All_HDU : HDU_Info_Arr(1 .. File.HDU_Cnt) :=
  -- File.HDU_Arr(1 .. File.HDU_Cnt).HDUInfo; FIXME why does not work ?
  (others=>Null_HDU_Info);
  Cnt : Positive := 1;
 begin
  while Cnt <= File.HDU_Cnt
  loop
   All_HDU(Cnt) := File.HDU_Arr(Cnt).HDUInfo;
   Cnt := Cnt + 1;
  end loop;
  return All_HDU;
 end List_HDUInfo;


  -----------------
  -- Data Access --
  -----------------

  -- Note: Alternative to variant record (DataArray_Type in fits_io.ads), is to use
  -- generic package which can be instantiated for all six FITS pre-defined types.

  -- FITS defines data of any dimensionality of 6 data types.
  -- At this user-level, the interface offers 1dimensional array of any of the 6 types.
  -- Positioning in this 1D-array (and so creating the N-dimensional space) is next level
  -- and not handled here. (FITS_IO.Data).
  -- Similarly, parsing the Header is in other, next level packages. (FITS_IO.Header)

  -- This level only checks that the written data is not longer then determined by Header,
  -- in particular: begining of the HDU (HDU_Pos.HeaderStart) and HDU_Pos.DataStart & .DataSize.
  -- Any attempt to write outside this region should raise exception.

  -- How to control truncate-at-write? When to allow, when to raise exception ?
  -- Higher-level issue or should be handled at this level ?

 function  Length( Data: in DataArray_Type) return Count
  is
   len : Count;
 begin
    case Data.Option is
     when Int8  =>   len := Data.Int8Arr'Length;
     when Int16 =>   len := Data.Int16Arr'Length;
     when Int32 =>   len := Data.Int32Arr'Length;
     when Int64 =>   len := Data.Int64Arr'Length;
     when Float32 => len := Data.Float32Arr'Length;
     when Float64 => len := Data.Float64Arr'Length;
    end case;
    return len;
 end Length;
 -- FIXME the need for this is weird...
 function  Size( dt: in BITPIX_Type) return Count
  is
   len : Count;
 begin
    case dt is
     when Int8  => len := Interfaces.Integer_8'Size;
     when Int16 => len := Interfaces.Integer_16'Size;
     when Int32 => len := Interfaces.Integer_32'Size;
     when Int64 => len := Interfaces.Integer_64'Size;
     when Float32 => len := Float'Size;
     when Float64 => len := Long_Float'Size;
    end case;
    return len;
 end Size;
  -- FIXME the need for this is weird...

 -------------------------------------------
 -- user level Read/Write: works by DataType

 -- FIXME should this check be done in To_SIO ? e.g. would do both: convert and check
 function  Is_Inside_DU(File     : in File_Type;
                        HDU_Num  : in Positive;  -- 1,2,3...
                        Start    : in Positive_Count;  -- first DataType position where Write/Read
                        DataType : in BITPIX_Type;     -- DataType to dtermine size
                        Length   : in Positive_Count)  -- number of DataType elements written/read
  return Boolean
 is
  DataEnd : Positive_Count := Start + (Size(DataType)/8) * Length;
  DUEnd   : Positive_Count := Start + BlockSize * File.HDU_Arr(HDU_Num).HDUPos.DataSize;
  Inside  : Boolean  := DataEnd <= DUEnd ;
 begin
  return Inside;
 end Is_Inside_DU;


 -- convert HDU-Offset to SIO file position, relative to begining of the file
 function  To_SIO(File     : in File_Type;
                  HDU_Num  : in Positive;  -- 1,2,3...
                  DataType : in BITPIX_Type;
                  Offset   : in Positive_Count)  -- in units of DataType
  return Positive_Count
 is
  -- FIXME BlockSize must be in units of Stream Element_Size
  DUStart    : Positive_Count := 1 + BlockSize * (File.HDU_Arr(HDU_Num).HDUPos.DataStart - 1);
  DataOffset : Count := (Size(DataType)/8) * (Offset - 1);
  SioOffset  : Positive_Count := DUStart + DataOffset;
 begin
   -- FIXME check HDU_Num not outside of the range?
  return SioOffset;
 end To_SIO;


 -- from HDU_Num DataUnit, read Length number of DataType
 -- from FromOffset relative to begining of the DataUnit
 function  Read (File       : in File_Type;
                 HDU_Num    : in Positive;        -- 1,2,3...
                 DataType   : in BITPIX_Type;
                 FromOffset : in Positive_Count;  -- in units of DataType
                 Length     : in Positive_Count ) -- in units of DataType
  return DataArray_Type
 is
  Data : DataArray_Type(DataType,Length);
  -- calculate SIO-Offset from ToOffset
  To   : Positive_Count := To_SIO(File,HDU_Num,DataType,FromOffset);
 begin

   if not Is_Inside_DU(File,HDU_Num,To,DataType,Length)
   then
     null;
     -- FIXME check if [HDU_Num,ToOffset,Data'Length] is
     -- within DataUnit limits. Raise exception if attempting to write
     -- outside this file-area.
   end if;

   Set_Index (File.BlocksFile, To);
   -- Fits_IO.Count inhereted from SIO -> SIO.Set_Index inherited for Fits_IO.Count

   DataArray_Type'Read( SIO.Stream(File.BlocksFile), Data );
  return Data;
 end Read;

 -- into HDU_Num DataUnit, write the Data
 -- to ToOffset relative to begining of the DataUnit
 -- Data must fit into the DataUnit as defined by the Header
 procedure Write (File     : in File_Type;
                  HDU_Num  : in Positive;        -- 1,2,3...
                  ToOffset : in Positive_Count;  -- in units of DataType
                  Data     : in DataArray_Type ) -- has length Length(Data)
 is
  -- calculate SIO-Offset from ToOffset
  To : Positive_Count := To_SIO(File,HDU_Num,Data.Option,ToOffset);
 begin

   if not Is_Inside_DU(File,HDU_Num,To,Data.Option,Length(Data))
   then
     null;
     -- FIXME check if [HDU_Num,ToOffset,Data'Length] is
     -- within DataUnit limits. Raise exception if attempting to write
     -- outside this file-area.
   end if;

   Set_Index (File.BlocksFile, To);
   -- Fits_IO.Count inhereted from SIO -> SIO.Set_Index inherited for Fits_IO.Count

   -- FIXME clarify: will this write also Data.Option (and Length) ?
   -- worakaround is to use Pragma C-style which creates a
   -- C-style union and so Option is not written only data:
   -- add these after record definition:
   -- pragma Unchecked_Union (DataArray_Type);
   -- pragma Convention (C, Union);    -- optional, only if interfacing C-lang
   DataArray_Type'Write( SIO.Stream(File.BlocksFile), Data );
 end Write;

end FITS_IO;
