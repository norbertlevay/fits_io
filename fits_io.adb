
-- FITS_IO implementation notes
--
-- Current implementation is based on two elements:
--
-- 1, Low-level FITS-file access (FITS_IO.Block_IO)
-- This presents a FITS-file as array of blocks, each block having 2880 octets.
-- The layer provides positioning by blocks, read and write.
--
-- 2, FITS-File HDU-lists
-- This is a vector of structures which store HDU's positions and sizes
-- (and other 'useful' info). It is part of File-handle record (FIST_IO.FITS_File_Type)
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
 -- for HDU dynamic vector implementation; unlike static array,
 -- dynamic vector allows unlimited number of HDU's in FITS-file
 -- as required in FITS standard

with FITS_IO.Block_IO;
 -- low-level FITS-file Header access

package body FITS_IO is

 package BIO renames FITS_IO.Block_IO;
 -- access FITS-file header in blocks of 2880 bytes [FITS]

 -- NOTE Count & Positive_Count defs moved to ads for Data access Read/Write params
-- type Count is new SIO.Count;
 -- will inherit Index() Set_Index() from SIO for Fits_IO.Positive_Count
 -- OR like in Ada.Direct_IO  - System.Direct_IO:
 -- type Count is range 0 .. Ada.Streams.Stream_IO.Count'Last;
 -- but this will not inherit Index Set_Index calls and conversion needed
-- subtype Positive_Count is Count range 1 .. Count'Last;
 -- FIXME we use Count whenever zero value is needed - consider introduce Natural_Count ?
 -- Why not done in standard *_IO packages? computaions may result in negative numbers?

  --------------------------
  -- FITS-File HDU vector --
  --------------------------

 -- HDU Records
 -- It is implemented as dynamic vector of ChunkSize = 3..5
 -- record-chunks of HDU info about positions and sizes
 -- of HDU's in FITS File.
 -- Open and Write will insert HDU Records into the list
 -- Create initializes an empty list
 -- Close destroys the list

 type HDU_Pos_Type is record
  HeaderStart : Positive_Count; -- BlockIndex where header starts with the FitsFile
  HeaderSize  : Positive_Count; -- Header size in Blocks (0 not allowed, HDU starts always with Header)
  DataStart   : Positive_Count; -- BlockIndex where header starts with the FitsFile
  DataSize    : Count;    -- Data size in Blocks (0 allowed, Primary HDU may have no data)
 end record;

 type HDU_Data_Type is record
  HDUPos  : HDU_Pos_Type;  -- internal data
  HDUInfo : HDU_Info_Type; -- part of external API (.ads)
 end record;

 -- HDU dynamic vector implementation
 -- search for: HDUV and HDUVect
 -- implemented in:
 -- * Parse_HDU_Positions -> Append all HDUs
 -- * Create/Open  -> init HDUVect
 -- * List_HDUInfo -> iterate and print each HDU_Info_Type record

 package HDUV is new Ada.Containers.Vectors
 	( Element_Type => HDU_Data_Type,
 	  Index_Type   => Positive);  -- HDU_Num = 1,2,3...

 HDUVectChunkSize : constant Ada.Containers.Count_Type := 3;
 -- vector chunks of size 3

 procedure HDUVect_init ( v : in out HDUV.Vector) is
 begin
   HDUV.Reserve_Capacity(v, HDUVectChunkSize);
 end;

 procedure HDUVect_debug ( v : in HDUV.Vector) is
  procedure printout( c : HDUV.Cursor) is
    HDUInfo : HDU_Info_Type := HDUV.Element(c).HDUInfo;
    HDUPos  : HDU_Pos_Type  := HDUV.Element(c).HDUPos;
  begin
    Ada.Text_IO.Put_Line("HDUVect Cards: " & Integer'Image(HDUInfo.CardsCnt));
    Ada.Text_IO.Put("HDUVect H: " & Positive_Count'Image(HDUPos.HeaderStart));
    Ada.Text_IO.Put(" ("   & Count'Image(HDUPos.HeaderSize) & ") ");
    Ada.Text_IO.Put(" DU: "   & Positive_Count'Image(HDUPos.DataStart));
    Ada.Text_IO.Put_Line(" (" & Count'Image(HDUPos.DataSize) & ")");
  end;
  FileSize : Positive_Count;
 begin
   Ada.Text_IO.Put_Line("HDUVect Length / Capacity: " & Ada.Containers.Count_Type'Image(HDUV.Length(v)) &
                                                " / " & Ada.Containers.Count_Type'Image(HDUV.Capacity(v)));
   HDUV.Iterate(v,printout'Access);

   FileSize := (HDUV.Last_Element(v).HDUPos.DataStart - 1 +
                HDUV.Last_Element(v).HDUPos.DataSize)*BIO.BlockSize;
   Ada.Text_IO.Put_Line("LastDU LastByte (=FileSize): " & Count'Image(FileSize));
 end;

 --
 -- File handler
 --
 type FITS_File_Type_Record is record
  BlocksFile : BIO.File_Type;
  Mode       : FITS_File_Mode;   -- FITS_IO.Mode
  HDUVect    : HDUV.Vector; -- dynamic vector implementation HDUVect
 end record;

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
      DUSize := 1 + DUSize/BIO.BlockSize;
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
 procedure Parse_KeyRecord( Card : in BIO.Card_Type;
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
 procedure  Parse_HeaderBlock( Block     : in BIO.HeaderBlock_Type ;
                               CardCount : out Positive;
                               HDUInfo   : in out HDU_Info_Type;
                               ENDFound  : in out Boolean )
--  return Boolean
 is
--  ENDFound : Boolean := False;
  Card     : BIO.Card_Type;
  CardCnt  : Positive := 1;
  TotCardsInBlock : Positive := Positive(BIO.CardsCntInBlock);
   -- Positive() conv ok, it is const 36
 begin
  ENDFound := False;

  for I in Block'Range loop
    Card := Block( I );
    Parse_KeyRecord( Card, HDUInfo );
    ENDFound := (Card = BIO.ENDCard);
    exit when ENDFound or CardCnt >= TotCardsInBlock;
    CardCnt := CardCnt + 1;
  end loop;
  CardCount := CardCnt;
--  return ENDFound;
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
  HDUPos.HeaderSize  := 1 + Positive_Count(HDUInfo.CardsCnt) / BIO.CardsCntInBlock;
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
 procedure Parse_HDU_Positions ( File : in out FITS_File_Type )
 is
   -- controlling the loops
   HDU_Cnt : Positive := 1;
   EndCardFound : Boolean := false;

   -- positions & size in file
   HeadStart_Index : Positive_Count;
   Next_HDU_Index  : Positive_Count;

   HDUInfo : HDU_Info_Type;
   Block : BIO.HeaderBlockArray_Type(1..1);

   CurCardCnt : Positive; -- written by Parse_HeaderBlock represents card-slots read/parsed
   TotCardCnt : Natural := 0;

   locHDUV : HDU_DATA_Type;-- for HDUVect implementation
 begin

   while not BIO.End_OF_File(File.BlocksFile)
   loop

      HDUInfo := Null_HDU_Info;
      TotCardCnt := 0;

      -- Header

      HeadStart_Index := BIO.BlockIndex( File.BlocksFile );

      loop
         Block        := BIO.Read( File.BlocksFile );
          -- raises exception when EOF reached, if not FITS-file
          -- FIXME ? it is ok, but might waste long time parsing big non-FITS file
--         EndCardFound := Parse_HeaderBlock( Block(1) , CurCardCnt, HDUInfo );
         Parse_HeaderBlock( Block(1) , CurCardCnt, HDUInfo, EndCardFound );
         TotCardCnt   := TotCardCnt + CurCardCnt;
         exit when EndCardFound ;
      end loop;

      -- DataUnit

      HDUInfo.CardsCnt := TotCardCnt; -- FIXME Natural to Positive conversion

      -- HDU info cllected now store it to FITS_File_Type
      locHDUV.HDUInfo := HDUInfo;
      locHDUV.HDUPos  := HDU_Info2Pos(HeadStart_Index, HDUInfo);
      HDUV.Append(File.HDUVect, locHDUV);

      -- skip data unit for next HDU-read

      Next_HDU_Index := HDUV.Element(File.HDUVect,HDU_Cnt).HDUPos.DataStart + HDUV.Element(File.HDUVect,HDU_Cnt).HDUPos.DataSize;
      BIO.Set_BlockIndex( File.BlocksFile, Next_HDU_Index );
      -- FIXME is HDU_Cnt really needed with HDUVect solution?

      HDU_Cnt := HDU_Cnt + 1;
      -- next HDU

   end loop;

   -- HDUVect_debug(File.HDUVect);

  end Parse_HDU_Positions;

 --
 -- for Write/Append - creating new HDU from Header
 --
 -- Open/Create inits File.HDU_Arr <- sets correct File-Index
 -- Write takes and converts Header_Type --> BIO.HeaderBlockArray_Type
 -- writes the header into the file
 -- if write success calls this Parse_HDU_Info & HDU_Info2Pos
 function  Parse_HDU_Info( HeaderBlocks : in BIO.HeaderBlockArray_Type)
  return HDU_Info_Type
 is
  HDUInfo  : HDU_Info_Type;
  ENDFound : Boolean := False;
  CurCardCnt : Positive; -- written by Parse_HeaderBlock represents card-slots read/parsed
  TotCardCnt : Natural := 0;
 begin

   for I in HeaderBlocks'Range
    loop
--     EndFound   := Parse_HeaderBlock( HeaderBlocks(I) , CurCardCnt, HDUInfo );
     Parse_HeaderBlock( HeaderBlocks(I) , CurCardCnt, HDUInfo, EndFound );
     TotCardCnt := TotCardCnt + CurCardCnt;
     exit when ENDFound;
   end loop;

   HDUInfo.CardsCnt := TotCardCnt;

   return HDUInfo;
 end Parse_HDU_Info;

 function  To_BIO( Mode : in FITS_File_Mode ) return BIO.File_Mode is
  BIO_Mode : BIO.File_Mode;
 begin

  case Mode is
    when In_File     => BIO_Mode := BIO.In_File;
    when Inout_File  => BIO_Mode := BIO.Inout_File;
    when Out_File    => BIO_Mode := BIO.Out_File;
    when Append_File => BIO_Mode := BIO.Append_File;
  end case;

  return BIO_Mode;
 end To_BIO;

 ---------------
 -- Interface --
 ---------------

 procedure Open ( File : in out FITS_File_Type;
                  Mode : in FITS_File_Mode;
                  Name : in String;
                  Form : in String   := "shared=no")
 is
 begin
  File := new FITS_File_Type_Record;
  BIO.Open( File.BlocksFile,
            BIO.In_File,
            Name,Form);--[GNAT,9.2 FORM strings]
  HDUVect_init(File.HDUVect);
  Parse_HDU_Positions ( File ); -- Fills in HDU data to File
  BIO.Set_Mode(File.BlocksFile,To_BIO(Mode));
  File.Mode := Mode;
 end Open;

 --
 --
 --
 procedure Close ( File : in out FITS_File_Type ) is
  procedure Delete_FileType is new Ada.Unchecked_Deallocation
                                            (FITS_File_Type_Record, FITS_File_Type);
 begin
  BIO.Close(File.BlocksFile);
  -- FIXME how to destroy dynamic-Vector ? Needed at all/garbageCollector? For now do only Clear()
  -- Ada.Finalization.Finalize(File.HDUVect);
  HDUV.Clear(File.HDUVect);
  Delete_FileType(File);
 end Close;

 --
 -- convert external Header represenation to FITS-internal format:
 -- * add space so that each card is 80 chars long
 -- * make sure space after END card is cleaned
 --
 function  To_HeaderBlockArray( Header : Header_Type ) return BIO.HeaderBlockArray_Type
 is
  -- init blocks with space-characters
  HB : BIO.HeaderBlockArray_Type(1 .. (1 + (Header'Length -1) / BIO.CardsCntInBlock)) := (others => BIO.EmptyBlock);
  Ix : Positive_Count := 1; -- index in header
  BIx, Cix : Count; -- block-index, card-index
 begin
  for I in Header'Range loop
    BIx := 1 + (Ix-1) /  BIO.CardsCntInBlock;
    CIx := Ix mod BIO.CardsCntInBlock;
    if CIx = 0 then
     CIx := BIO.CardsCntInBlock;
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
 -- FIXME is this good idea at all -> Better use separate Append(File,Header) besides Write(File,Header,HDU_Num)
 procedure Write ( File    : in out FITS_File_Type;
                   Header  : in Header_Type;
                   HDU_Num : in Positive := HDU_AfterLast )-- default: Append
 is
  HeaderBlocks : BIO.HeaderBlockArray_Type := To_HeaderBlockArray(Header);
  HDUInfo      : HDU_Info_Type := Parse_HDU_Info( HeaderBlocks );
  CurMode      : FITS_File_Mode := File.Mode;
  HDUIx  : Positive;
  FileIx : Positive_Count;
  -- Fits File consists of sequence of HDU's of
  --   different size numbered by HDU-index: 1,2.3...
  -- File.HDU_Arr/HDUVector connects FileIx and HDUIx where:
  --   FileIx is position of the HDU in file given blocks
  --   HDUIx  is the HDU-index itself
  HDUData : HDU_Data_Type := (HDUInfo => HDUInfo, HDUPos => (1,1,1,0));
  use  Ada.Containers;
  DelCount : Ada.Containers.Count_Type;
 begin

  -- 0. Sanity checks on HDU_Num

  if HDUV.Is_Empty(File.HDUVect) and
     (HDU_Num /= 1)
  then
   null; -- raise exception and exit... UserMsg: to empty file we can write only 1st HDU
  end if;

  -- Client can supply max HDU_Num = (Last_Index + 1) in which case do as in Append_Mode
  if (HDU_Num > (HDUV.Last_Index(File.HDUVect) + 1)) and
     (HDU_Num /= HDU_AfterLast)
  then
   null; -- raise exception and exit... UserMsg: HDU_Num out of range for given file & mode combination
  end if;

  -- for Inout_Mode (e.g. file update) check sizes:
  -- space in File vs Header+Data as defined be the new Header
  -- sizes must match except when updating the last HDU if no other data follows.
  -- FIXME what if FITS file has non-standard extensions after last HDU ? Check FITS-standard whether possible.
  if ((HDU_Num < HDUV.Last_Index(File.HDUVect)) and (CurMode = Inout_File))
  then
    if HDUV.Element(File.HDUVect,HDU_Num).HDUPos.HeaderSize /= (Header'Length / BIO.CardsCntInBlock + 1)
    then
     null;
     -- FIXME raise exception if sizes don't match
     -- UserMsg: For updating HDU (Inout mode) HDU sizes must match.
    end if;
  end if;

  -- 1. Set HDUIx and FileIx depending on the situation

  if HDUV.Is_Empty(File.HDUVect)
  then
   -- writing to an empty file
   HDUIx  := 1;
   FileIx := 1;
  else
   -- updating existing (non-empty) file
   case CurMode is
     when Inout_File|Out_File =>
       HDUIx   := HDU_Num;
       HDUData := HDUV.Element(File.HDUVect,HDU_Num);
       FileIx  := HDUData.HDUPos.HeaderStart;
     when Append_File =>
       HDUIx   := HDUV.Last_Index(File.HDUVect) + 1;
       HDUData := HDUV.Last_Element(File.HDUVect);
       FileIx  := HDUData.HDUPos.DataStart + HDUData.HDUPos.DataSize;
     when others =>
       null;-- FIXME raise exception or let Stream_IO.Write raise exception?
   end case;
  end if;

  -- 2, Write data to file
  BIO.Set_BlockIndex(File.BlocksFile,FileIx);
  BIO.Write(File.BlocksFile, HeaderBlocks);
  -- FIXME write here last byte of the last-HDU ? (so guarantee
  -- correct FITS_file size even before data was written). Or leave it to DU-handling ?
  -- For FITS-file's last HDU/DU-unit: when should we create DU as multiple of
  -- BIO.BlockSize as given in Header ?
  -- Note: Currrently data-write, writes only N-data elements, which is correct
  -- (not to overwrite other data if DU was already initialized before).

  -- 3, Update File.HDUVect if write successful...
  HDUData.HDUPos := HDU_Info2Pos(FileIx,HDUInfo);
  if HDUV.Is_Empty(File.HDUVect) or
     (CurMode = Append_File)
  then
    HDUV.Append(File.HDUVect,HDUData);
  else

    if CurMode = Inout_File then
      HDUV.Replace_Element(File.HDUVect,HDUIx,HDUData);
    elsif CurMode = Out_File then
      -- Write-with-Out_File truncated the file at HDU_Num - 1 :
      -- delete corresponding elements from HDUVect and Append the new HDU
      DelCount := 1 + HDUV.Length(File.HDUVect) - Ada.Containers.Count_Type(HDUIx);
      HDUV.Delete_Last(File.HDUVect,DelCount);-- remove DelCount HDU-entries from end of the vector
      HDUV.Append(File.HDUVect,HDUData);
    end if;

  end if;

 end Write;

 -- Create FitsFile
 -- if Mode Out_File   : write will create first HDU of a new file
 -- if Mode Append_Mode: write will create new HDU at the end of a file
 -- FIXME check Create + Append behaviour: 2nd case should map to OpenFile in Append_Mode ?
 procedure Create ( File : in out FITS_File_Type;
                    Mode : in FITS_File_Mode;
                    Name : in String;
                    Form : in String    := "shared=no") is
 begin
  File := new FITS_File_Type_Record;
  BIO.Create( File.BlocksFile,
              To_BIO(Mode),
              Name,Form);
  HDUVect_init(File.HDUVect);
  BIO.Set_Mode(File.BlocksFile,To_BIO(Mode));
  File.Mode := Mode;
 end Create;

 function  CardsCount (HeaderBlocks : BIO.HeaderBlockArray_Type) return Positive
 is
  Count : Positive := 1;
 begin
  for I in HeaderBlocks'Range loop
   for J in HeaderBlocks(I)'Range loop
    exit when HeaderBlocks(I)(J) = BIO.ENDCard;
    Count := Count + 1;
   end loop;
  end loop;
  return Count;
 end CardsCount;
 -- FIXME we need CardsCount stored in the BIO.HeaderBlockArray_Type


 --
 -- convert internal Header format to external Header representation
 -- * strip empty spaces from start end end of each line
 -- * strip empty cards after END-card
 --
 function  To_Header( HeaderBlocks : BIO.HeaderBlockArray_Type ) return Header_Type
 is
  H  : Header_Type(1 .. CardsCount(HeaderBlocks));
  Ix : Positive := 1;
 begin
  for I in HeaderBlocks'Range loop
   for J in HeaderBlocks(I)'Range loop
    H(Ix) := Card.Trim(Card.To_Bounded_String(HeaderBlocks(I)(J)) , Ada.Strings.Both);
    exit when HeaderBlocks(I)(J) = BIO.ENDCard;
    Ix := Ix + 1;
   end loop;
  end loop;
  return H;
 end To_Header;

 --
 -- Read header from FITS-file
 --
 function  Read ( File    : in  FITS_File_Type;
                  HDU_Num : in  Positive ) return Header_Type
 is
  Header   : Header_Type(1..HDUV.Element(File.HDUVect,HDU_Num).HDUInfo.CardsCnt);
 begin

  -- sanity check (like in Write)
  if (HDU_Num > HDUV.Last_Index(File.HDUVect)) and
     (HDU_Num /= HDU_AfterLast)
  then
   null; -- raise exception and exit... UserMsg: HDU_Num out of range for given file & mode combination
  end if;

  -- position by HDU_Num in file
  -- FIXME consider API Set_HDUIndex() -> and Read() Write() without HDU_Num
  BIO.Set_BlockIndex(File.BlocksFile, HDUV.Element(File.HDUVect,HDU_Num).HDUPos.HeaderStart);

  -- read and convert to Header
  declare
    HeaderBlocks : BIO.HeaderBlockArray_Type := BIO.Read (File.BlocksFile, HDUV.Element(File.HDUVect,HDU_Num).HDUPos.HeaderSize);
  begin
    Header := To_Header(HeaderBlocks);
  end;

  return Header;
 end Read;

 --
 --
 --
 procedure List_HDUInfo (File : in FITS_File_Type;
                         Print: not null access
                           procedure(HDUInfo : HDU_Info_Type; Index : Positive))
 is
  procedure printout( c : HDUV.Cursor) is
    HDUInfo : HDU_Info_Type := HDUV.Element(c).HDUInfo;
  begin
    Print(HDUInfo,Positive(HDUV.To_Index(c))); -- FIXME conversion Positive
  end;
 begin
   HDUV.Iterate(File.HDUVect,printout'Access);
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
 function  Is_Inside_DU(File     : in FITS_File_Type;
                        HDU_Num  : in Positive;  -- 1,2,3...
                        Start    : in Positive_Count;  -- first DataType position where Write/Read
                        DataType : in BITPIX_Type;     -- DataType to dtermine size
                        Length   : in Positive_Count)  -- number of DataType elements written/read
  return Boolean
 is
  DataEnd : Positive_Count := Start + (Size(DataType)/8) * Length;
  DUEnd   : Positive_Count := Start + BIO.BlockSize * HDUV.Element(File.HDUVect,HDU_Num).HDUPos.DataSize;
  Inside  : Boolean  := DataEnd <= DUEnd ;
 begin
  return Inside;
 end Is_Inside_DU;


 -- convert HDU-Offset to SIO file position, relative to begining of the file
 function  To_SIO(File     : in FITS_File_Type;
                  HDU_Num  : in Positive;  -- 1,2,3...
                  DataType : in BITPIX_Type;
                  Offset   : in Positive_Count)  -- in units of DataType
  return Positive_Count
 is
  -- FIXME BIO.BlockSize must be in units of Stream Element_Size
  DUStart    : Positive_Count := 1 + BIO.BlockSize * (HDUV.Element(File.HDUVect,HDU_Num).HDUPos.DataStart - 1);
  DataOffset : Count := (Size(DataType)/8) * (Offset - 1);
  SioOffset  : Positive_Count := DUStart + DataOffset;
 begin
   -- FIXME check HDU_Num not outside of the range?
  return SioOffset;
 end To_SIO;


 -- from HDU_Num DataUnit, read Length number of DataType
 -- from FromOffset relative to begining of the DataUnit
 function  Read (File       : in FITS_File_Type;
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

   DataArray_Type'Read( BIO.Stream(File.BlocksFile), Data );
  return Data;
 end Read;

 -- into HDU_Num DataUnit, write the Data
 -- to ToOffset relative to begining of the DataUnit
 -- Data must fit into the DataUnit as defined by the Header
 procedure Write (File     : in FITS_File_Type;
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
   DataArray_Type'Write( BIO.Stream(File.BlocksFile), Data );
 end Write;

end FITS_IO;
