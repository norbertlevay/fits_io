
with Ada.Text_IO;


with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Bounded;   use Ada.Strings.Bounded;

with Ada.Unchecked_Deallocation;

with Mandatory;
with File; use File;
with File_Funcs;
with Keyword_Record;  use Keyword_Record;


package body Header is

    package TIO renames Ada.Text_IO;
    package KW renames Keyword_Record;


    CardsCntInBlock : constant Positive := 36;
    -- FIXME Card_Block is used in File.Misc::Copy_Blocks & Copy_HDU
    -- find other solution for File.Misc, here move it inside body
    type Card_Block is array (Positive range 1..CardsCntInBlock) of String_80;
    pragma Pack (Card_Block);
    -- FIXME does Pack guarantee arr is packed? how to guarantee Arrs are packed
    -- OR do we need to guarantee at all ?






   --
   -- Read File until ENDCard found
   --

   procedure Read_Card (File : in SIO.File_Type;
            HStart : in SIO.Positive_Count;
                        CurBlkNum : in out SIO.Count;
                        Blk : in out Card_Block;
                        CardNum : in SIO.Positive_Count;
                        Card    : out  String)
   is
         BlockSize_SIOunits : constant SIO.Positive_Count := 2880;
         BlkNum : SIO.Positive_Count;
         CardNumInBlk : Natural;
         BlkNumIndex : SIO.Positive_Count;
   begin
         BlkNum := 1 + (CardNum - 1) / 36; 
         CardNumInBlk := Natural(CardNum - (BlkNum - 1) * 36);-- FIXME is < 36 Natural is ok if two lines together

         if(BlkNum /= CurBlkNUm)
         then
                        -- FIXME BEGIN only this section depends on SIO. file access
                        -- make it Read_Block(SIO.File, FileBlkNum, Blk)
                        -- where FileBlkNum := HStart + BlkNum
                        -- BlkNum - relative to HDU start
                        -- FileBlkNum - relative to File start
               BlkNumIndex :=  HStart + (BlkNum-1) * BlockSize_SIOunits;

               SIO.Set_Index(File, BlkNumIndex);
               Card_Block'Read(SIO.Stream(File), Blk);
               CurBlkNum := BlkNum;
               -- FIXME END   only this section depends on SIO. file access
         end if;

         Card := Blk(CardNumInBlk);

   end Read_Card;


-- read info help in Mandatory keys of the Header
  function  Read_Mandatory (FitsFile : in SIO.File_Type) return Mandatory.Result_Rec
  is
        HeaderStart : SIO.Positive_Count := SIO.Index(FitsFile);    
                CardNum : SIO.Count;
                Card : String(1..80);

                CurBlkNum : SIO.Count := 0; -- none read yet
                Blk : Card_Block;
   begin
                CardNum := Mandatory.Reset_State;
                loop
                        Read_Card(FitsFile, HeaderStart, CurBlkNum, Blk, CardNum, Card);
                        CardNum := Mandatory.Next(CardNum, Card);
                        exit when (CardNum = 0); 
                end loop;

                -- calc HDU size

                declare
                        PSize : Mandatory.Result_Rec := Mandatory.Get;
                begin
            return PSize;
                end;

   end Read_Mandatory;


   function  Read_Optional (FitsFile : in  SIO.File_Type;
                       Keys : in Optional.Bounded_String_8_Arr)
      return Card_Arr
   is
    HeaderStart : SIO.Positive_Count := SIO.Index(FitsFile);    
        CardNum : SIO.Count;
        Card : String(1..80);

    CurBlkNum : SIO.Count := 0; -- none read yet
    Blk : Card_Block;
   begin
        CardNum := Optional.Init(Keys);
        loop
        Read_Card(FitsFile, HeaderStart, CurBlkNum, Blk, CardNum, Card);
                CardNum := Optional.Next(CardNum, Card);
                exit when (CardNum = 0); 
        end loop;


    declare
        Cards : Card_Arr := Optional.Get_Cards;
    begin
        return Cards;
    end;

   end Read_Optional;





end Header;

