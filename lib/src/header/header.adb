
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
   
   --
   -- Read File until ENDCard found
   --

   procedure Read_Card (File : in SIO.File_Type;
            HStart : in SIO.Positive_Count;
                        CurBlkNum : in out Natural;
                        Blk : in out Card_Block;
                        CardNum : in Positive;
                        Card    : out  String)
   is
     BlockSize_SIOunits : constant SIO.Positive_Count := 2880;
         BlkNum : Positive;
         CardNumInBlk : Positive;
         BlkNumIndex : SIO.Positive_Count;
   begin
         BlkNum := 1 + (CardNum - 1) / 36; 
         CardNumInBlk := CardNum - (BlkNum - 1) * 36; 
    
         if(BlkNum /= CurBlkNUm)
         then
                        -- FIXME BEGIN only this section depends on SIO. file access
                        -- make it Read_Block(SIO.File, FileBlkNum, Blk)
                        -- where FileBlkNum := HStart + BlkNum
                        -- BlkNum - relative to HDU start
                        -- FileBlkNum - relative to File start
               BlkNumIndex := SIO.Positive_Count( Positive(HStart) + (BlkNum-1) 
                                                * Positive(BlockSize_SIOunits) );

               SIO.Set_Index(File, BlkNumIndex);
               Card_Block'Read(SIO.Stream(File), Blk);
               CurBlkNum := BlkNum;
               -- FIXME END   only this section depends on SIO. file access
         end if;

         Card := Blk(CardNumInBlk);

   end Read_Card;


-- read info help in Mandatory keys of the Header
  function  Read_Header (FitsFile : in SIO.File_Type) return Mandatory.Result_Rec
  is
        HeaderStart : SIO.Positive_Count := SIO.Index(FitsFile);    
                CardNum : Natural;
                Card : String(1..80);

                CurBlkNum : Natural := 0; -- none read yet
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

   end Read_Header;


   function  Read_Header (FitsFile : in  SIO.File_Type;
                       Keys : in Optional.Bounded_String_8_Arr)
      return Card_Arr
   is
    HeaderStart : SIO.Positive_Count := SIO.Index(FitsFile);    
        CardNum : Natural;
        Card : String(1..80);

    CurBlkNum : Natural := 0; -- none read yet
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

   end Read_Header;





end Header;

