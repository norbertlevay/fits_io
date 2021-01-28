

-- NOTE on possible internal structures to represent a Header:
-- Mandatory.Result_Rec
-- Arrays               -- max Length Card_Arr for all categories of Reserved keys
-- Observation
-- Biblio
-- <all Reserved categories as of V3 Standard>  NOTE Version3 dependency!?
-- Buffer_COMMENTS      -- Buffer represents a fixed size record needed to maintain a buffer
-- Buffer_HISTORY       -- to read COMMENTS/HISTORY/Optional cards from Header on demand
-- Buffer_Optional_Proprietary

-- Cards in Header are of 3 categories:
-- No limit:
-- cards whose count has no limit: COMMENTS HISTORY and Optional-proprietary
-- have no limit in Card_Arr'Length so only a buffer
-- to those cards in File can be build, ultimately they are always accesed from File
-- Limit on Max-count:
-- Cards category whose count has max-limit: Reserved
-- Fixed size:
-- Mandatory cards can be mapped to fixed size record

-- As Internal model:
-- These structures can be viewed as internal model of the Header - they become state variables
-- and must be kept consistent with Header at any moment of execution
-- (Read Writes even in parallel: locking the file)
-- OR
-- As Optimization:
-- if restricted their use to Read-only Open (e.g. Header cannot change) they serve as buffers for
-- access/speed optimization at Reads
-- FIXME how about Writes ?


with Ada.Text_IO;


with FITS; use FITS;
--with FITS_IO; use FITS_IO;
with Ada.Streams.Stream_IO;-- use Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;     --use Ada.Strings.Fixed;
with Ada.Strings.Bounded;   --use Ada.Strings.Bounded;
with Ada.Strings; use Ada.Strings;
with Ada.Unchecked_Deallocation;

--with V3_Types;   use V3_Types; -- Float_32 needed
with Mandatory;
with File; use File;
with File.Misc; -- needs Write_Padding for Header
with File_Funcs;
--with Keyword_Record;  use Keyword_Record;


with Card;

package body Header is

   package TIO renames Ada.Text_IO;

   CardsCntInBlock : constant Positive := 36;
   -- FIXME Card_Block is used in File.Misc::Copy_Blocks & Copy_HDU
   -- find other solution for File.Misc, here move it inside body
   type Card_Block is array (Positive range 1..CardsCntInBlock) of String_80;
   pragma Pack (Card_Block);
   -- FIXME does Pack guarantee arr is packed? how to guarantee Arrs are packed
   -- OR do we need to guarantee at all ?


   -- buffered Read

   procedure Read_Card (File : in SIO.File_Type;
      HStart : in Positive_Count;
      CurBlkNum : in out Count;
      Blk : in out Card_Block;
      CardNum : in Positive_Count;
      Card    : out  String)
   is
      BlockSize_SIOunits   : constant Positive_Count := 2880;
      BlkNum               : Positive_Count;
      CardNumInBlk         : Natural;
      BlkNumIndex          : Positive_Count;
   begin
      BlkNum := 1 + (CardNum - 1) / 36; 
      CardNumInBlk := Natural(CardNum - (BlkNum - 1) * 36);
      -- FIXME is < 36 Natural is ok if two lines together

      if(BlkNum /= CurBlkNUm)
      then
         -- FIXME BEGIN only this section depends on SIO. file access
         -- make it Read_Block(SIO.File, FileBlkNum, Blk)
         -- where FileBlkNum := HStart + BlkNum
         -- BlkNum - relative to HDU start
         -- FileBlkNum - relative to File start
         BlkNumIndex :=  HStart + (BlkNum-1) * BlockSize_SIOunits;

         SIO.Set_Index(File, SIO.Count(BlkNumIndex));
         -- FIXME cast
         Card_Block'Read(SIO.Stream(File), Blk);
         CurBlkNum := BlkNum;
         -- FIXME END   only this section depends on SIO. file access
      end if;

      Card := Blk(CardNumInBlk);

   end Read_Card;



   -- Parse Header Elements

   function  Read_Mandatory (FitsFile : in SIO.File_Type) return Mandatory.Result_Rec
   is
      HeaderStart : Positive_Count := Count(SIO.Index(FitsFile));--FIXME cast
      CardNum     : Count;
      Card        : String(1..80);
      CurBlkNum   : Count := 0; -- none read yet
      Blk         : Card_Block;
   begin

      CardNum := Mandatory.Reset_State;
      loop
         Read_Card(FitsFile, HeaderStart, CurBlkNum, Blk, CardNum, Card);
         CardNum := Mandatory.Next(CardNum, Card);
         exit when (CardNum = 0); 
      end loop;

      declare
         PSize : Mandatory.Result_Rec := Mandatory.Get;
      begin
         return PSize;
      end;

   end Read_Mandatory;



   function  Read_Optional (FitsFile : in  SIO.File_Type;
      Keys : in Optional.Bounded_String_8_Arr)
      return Optional.Card_Arr
   is
      HeaderStart : Positive_Count := Positive_Count(SIO.Index(FitsFile));-- FIXME cast
      CardNum     : Count;
      Card        : String(1..80);
      CurBlkNum   : Count := 0; -- none read yet
      Blk         : Card_Block;
   begin

      CardNum := Optional.Init(Keys);
      loop
         Read_Card(FitsFile, HeaderStart, CurBlkNum, Blk, CardNum, Card);
         CardNum := Optional.Next(CardNum, Card);
         exit when (CardNum = 0); 
      end loop;

      declare
         Cards : Optional.Card_Arr := Optional.Get_Cards;
      begin
         return Cards;
      end;

   end Read_Optional;



   -- Compose Header Elements

   procedure Write_ENDCard_With_Padding(FFile : SIO.File_Type)
   is
   begin
      String_80'Write(SIO.Stream(FFile), ENDCard);
      File.Misc.Write_Padding(FFile, SIO.Index(FFile), File.Misc.HeaderPadValue);
   end Write_ENDCard_With_Padding;


end Header;

